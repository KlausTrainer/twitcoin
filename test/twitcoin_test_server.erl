%% @doc Test server for twitcoin.
-module(twitcoin_test_server).

-export([start/2, stop/0, loop/3, set_balance/2]).
-export([test_wallet_passphrase/0]).

%% External API

start(RpcUser, RpcPassword) ->
    Options = [{ip, {127,0,0,1}}, {port, 0}, {name, testserver}],
    ets:new(data, [public, named_table]),
    register(wallet_locker, spawn(fun wallet_locker/0)),
    Loop = fun (Req) ->
               "/" = Req:get(path) ,
               'POST' = Req:get(method),
               ?MODULE:loop(Req, RpcUser, RpcPassword)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    wallet_locker ! stop,
    mochiweb_http:stop(?MODULE).

loop(Req, RpcUser, RpcPassword) ->
    <<"Basic ", Creds/binary>> =
        list_to_binary(Req:get_header_value("authorization")),
    [RpcUser, RpcPassword] = string:tokens(binary_to_list(base64:decode(Creds)), ":"),
    {Object} = jiffy:decode(Req:recv_body()),
    Method = proplists:get_value(<<"method">>, Object),
    Params = proplists:get_value(<<"params">>, Object),
    Id = proplists:get_value(<<"id">>, Object),
    case Method of
    <<"getaccountaddress">> ->
        {ok, Address} = getaccountaddress(hd(Params)),
        Response = <<"{\"result\":\"", Address/binary, "\",\"error\":null,\"id\":\"", Id/binary, "\"}">>,
        Req:respond({200, [], Response});
    <<"getaccount">> ->
        {ok, Account} = getaccount(hd(Params)),
        Response = <<"{\"result\":\"", Account/binary, "\",\"error\":null,\"id\":\"", Id/binary, "\"}">>,
        Req:respond({200, [], Response});
    <<"getnewaddress">> ->
        {ok, Address} = getnewaddress(hd(Params)),
        Response = <<"{\"result\":\"", Address/binary, "\",\"error\":null,\"id\":\"", Id/binary, "\"}">>,
        Req:respond({200, [], Response});
    <<"getbalance">> ->
        {ok, Balance} = getbalance(hd(Params)),
        BalanceBin = list_to_binary(float_to_list(Balance)),
        Response = <<"{\"result\":", BalanceBin/binary, ",\"error\":null,\"id\":\"", Id/binary, "\"}">>,
        Req:respond({200, [], Response});
    <<"sendfrom">> ->
        [Account, Address, Amount] = Params,
        {Result, Code, ErrorMsg} = case sendfrom(Account, Address, Amount) of
        {ok, TxId} -> {<<"\"", TxId/binary, "\"">>, 200, <<"null">>};
        {error, Reason} -> {<<"null">>, 500, Reason}
        end,
        Response = <<"{\"result\":", Result/binary, ",\"error\":", ErrorMsg/binary, ",\"id\":\"", Id/binary, "\"}">>,
        Req:respond({Code, [], Response});
    <<"walletpassphrase">> ->
        [Passphrase, Timeout] = Params,
        WalletPassphrase = test_wallet_passphrase(),
        {Result, Code, ErrorMsg} = case binary_to_list(Passphrase) of
        WalletPassphrase ->
            wallet_locker ! {unlock_wallet, Timeout},
            {<<"null">>, 200, <<"null">>};
        _ ->
            {<<"null">>, 500, <<"{\"code\":-14,\"message\":\"Error: The wallet passphrase entered was incorrect.\"}">>}
        end,
        Response = <<"{\"result\":", Result/binary, ",\"error\":", ErrorMsg/binary, ",\"id\":\"", Id/binary, "\"}">>,
        Req:respond({Code, [], Response})
    end.

%% @spec set_balance(string(), float()) -> true
set_balance(Account, Balance) ->
    ets:insert(data, {{balance, list_to_binary(Account)}, Balance}).

test_wallet_passphrase() ->
    "test wallet passphrase".

%% @spec getaccountaddress(binary()) -> {ok, binary()}
getaccountaddress(Account) ->
    case ets:lookup(data, {addresses, Account}) of
    [] ->
        Addr = list_to_binary(integer_to_list(crypto:rand_uniform(0, 4000000000))),
        ets:insert(data, {{addresses, Account}, [Addr]}),
        ets:insert(data, {{accounts, Addr}, Account}),
        {ok, Addr};
    [{{addresses, _}, Addresses}] ->
        {ok, hd(Addresses)}
    end.

%% @spec getnewaddress(binary()) -> {ok, binary()}
getnewaddress(Account) ->
    Addr = list_to_binary(integer_to_list(crypto:rand_uniform(0, 4000000000))),
    ets:delete(data, {addresses, Account}),
    ets:insert(data, {{addresses, Account}, [Addr]}),
    ets:insert(data, {{accounts, Addr}, Account}),
    {ok, Addr}.

%% @spec getaccount(binary()) -> {ok, Account::binary()} | {error, Reason::binary()}
getaccount(Address) ->
    case ets:lookup(data, {accounts, Address}) of
    [] ->
        {error, <<"{\"code\":-5,\"message\":\"Invalid bitcoin address\"}">>};
    [{{accounts, _}, Account}] ->
        {ok, Account}
    end.

%% @spec getbalance(binary()) -> {ok, float()} | {error, Reason::binary()}
getbalance(Account) ->
    case ets:lookup(data, {balance, Account}) of
    [] -> {ok, 0.0};
    [{{balance, _}, Balance}] -> {ok, Balance}
    end.

%% @spec sendfrom(binary(), binary(), float()) -> {ok, TxId::binary} | {error, Reason::binary()}
sendfrom(SenderAccount, Address, Amount) ->
    {ok, SenderBalance} = getbalance(SenderAccount),
    case SenderBalance >= Amount of
    true ->
        {ok, ReceiverAccount} = getaccount(Address),
        {ok, ReceiverBalance} = getbalance(ReceiverAccount),
        ets:insert(data, {{balance, SenderAccount}, SenderBalance - Amount}),
        ets:insert(data, {{balance, ReceiverAccount}, ReceiverBalance + Amount}),
        {ok, list_to_binary(integer_to_list(crypto:rand_uniform(0, 4000000)))};
    false ->
        {error, <<"{\"code\":-4,\"message\":\"Insufficient funds\"}">>}
    end.

wallet_locker() ->
    receive
        stop ->
            ok;
        lock_wallet ->
            put(wallet_locked, true),
            wallet_locker();
        {unlock_wallet, Timeout} ->
            put(wallet_locked, false),
            erlang:send_after(Timeout * 1000, wallet_locker, lock_wallet),
            wallet_locker()
    end.
