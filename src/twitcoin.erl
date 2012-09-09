%% @doc twitcoin.
-module(twitcoin).

-export([start/0, start/1, stop/0, get_app_env/1, get_app_env/2]).

-export([get_account/3, get_address/1, send_from/4]).

-include("twitcoin.hrl").

-define(QRCODE_DIR, "/images/qrcodes/").


%% External API

%% @doc Start the twitcoin server.
-spec start() -> ok.
start() ->
    twitcoin_deps:ensure(),
    ensure_started(sasl),
    ensure_started(crypto),
    ensure_started(public_key),
    ensure_started(ssl),
    ensure_started(ibrowse),
    application:start(twitcoin).

%% @doc Start twitcoin.
%% `ConfigPath' specifies the location of the configuration file.
-spec start(ConfigPath :: string()) -> ok.
start(ConfigPath) ->
    application:set_env(twitcoin, configpath, ConfigPath),
    start().

%% @doc Stop the twitcoin server.
-spec stop() -> ok.
stop() ->
    application:stop(twitcoin).

%% @doc The official way to get the values set in twitcoin's configuration
%% file. Will return `undefined' if the given option is unset.
-spec get_app_env(atom()) -> term().
get_app_env(Opt) ->
    get_app_env(Opt, undefined).

%% @doc The official way to get the values set in twitcoin's configuration
%%      file. Will return `undefined' if the given option is unset.
-spec get_app_env(atom(), term()) -> term().
get_app_env(Opt, Default) ->
    case application:get_env(twitcoin, Opt) of
    {ok, Val} ->
        Val;
    _ ->
        case init:get_argument(Opt) of
        {ok, [[Val|_]]} -> Val;
        error -> Default
        end
    end.

-spec get_address(string() | binary()) ->
    {ok, Address :: binary} | {error, binary()}.
get_address(Account) ->
    case twitcoin_db:get_address(Account) of
    not_found ->
        case twitcoin_bitcoind_client:getaccountaddress(Account) of
        {error, Reason} ->
            {error, Reason};
        {ok, Address} ->
            twitcoin_db:put_address(Account, Address),
            {ok, Address}
        end;
    {ok, Address} ->
        {ok, Address}
    end.

-spec get_account(string(), string(), string() | binary()) ->
    {ok, account_data()} | {error, binary()}.
get_account(Token, TokenSecret, Account) when is_binary(Account) ->
    get_account(Token, TokenSecret, ?b2l(Account));
get_account(Token, TokenSecret, Account) ->
    case twitcoin_twitter_client:get_user_data(Token, TokenSecret, Account) of
    {error, Reason} ->
        {error, Reason};
    not_found ->
        twitcoin_db:delete_address(Account),
        {error, ?MSG_NO_TWITTER_ACCOUNT};
    {ok, {Name, ProfileImgUrl}} ->
        case get_address(Account) of
        {error, Reason} ->
            {error, Reason};
        {ok, Address} ->
            case twitcoin_bitcoind_client:getbalance(Account) of
            {error, Reason} ->
                {error, Reason};
            {ok, Balance} ->
                maybe_create_qrcode(Address),
                QRCodePath = <<?QRCODE_DIR,Address/binary,".png">>,
                Result = [
                    {address, Address},
                    {address_qrcode_path, QRCodePath},
                    {balance, Balance},
                    {name, Name},
                    {profile_img_url, ProfileImgUrl},
                    {screen_name, ?l2b(Account)}
                ],
                {ok, Result}
            end
        end
    end.

-spec send_from(string() | binary(), string() | binary(), float(), float()) ->
    {ok, TxId :: binary()} | {error, binary()}.
send_from(SenderAccount, ReceiverAddress, Amount, TxFee) ->
    case get_balance(SenderAccount) of
    {error, Reason} ->
        {error, Reason};
    {ok, Balance} when Balance < Amount + TxFee ->
        {error, ?MSG_INSUFFICIENT_FUNDS};
    {ok, _Balance} ->
        case twitcoin_bitcoind_client:sendfrom(SenderAccount, ReceiverAddress,
            Amount) of
        {error, Reason} ->
            {error, Reason};
        {ok, TxId} ->
            maybe_create_new_address(ReceiverAddress),
            {ok, TxId}
        end
    end.


%% Internal API

-spec maybe_create_qrcode(binary()) -> ok.
maybe_create_qrcode(Address) ->
    Filename = twitcoin_deps:local_path(["priv", "www", "static"])
        ++ ?QRCODE_DIR ++ ?b2l(Address) ++ ".png",
    case filelib:is_file(Filename) of
    true ->
        ok;
    false ->
        spawn(
            fun() ->
                QRCode = qrcode:encode(<<"bitcoin:",Address/binary>>, 'Q'),
                Image = qrcode_png_encoder:png_encode(QRCode),
                file:write_file(Filename, Image)
            end),
        ok
    end.

-spec get_balance(string() | binary()) -> {ok, Balance :: float()} | {error, binary()}.
get_balance(Account) ->
    case twitcoin_db:get_address(Account) of
    not_found -> {error, ?MSG_NO_TWITCOIN_ACCOUNT};
    {ok, _Address} -> twitcoin_bitcoind_client:getbalance(Account)
    end.

%% @doc Try to retrieve an account corresponding to the given address
%%      from the wallet. If an account could be found, create a new
%%      address for it.
-spec maybe_create_new_address(string() | binary()) -> ok.
maybe_create_new_address(Address) ->
    case twitcoin_bitcoind_client:getaccount(Address) of
    {error, _Reason} ->
        ok;
    {ok, Account} ->
        case twitcoin_bitcoind_client:getnewaddress(Account) of
        {error, _Reason} ->
            ok;
        {ok, NewAddress} ->
            twitcoin_db:put_address(Account, NewAddress)
        end
    end.

ensure_started(App) ->
    case application:start(App) of
    ok -> ok;
    {error, {already_started, App}} -> ok
    end.
