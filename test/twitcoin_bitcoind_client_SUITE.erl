-module(twitcoin_bitcoind_client_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include("twitcoin.hrl").


all() ->
	[
        {group, twitcoin_accounts},
        {group, twitcoin_wallet}
    ].

groups() ->
	[
        {twitcoin_accounts, [shuffle],
            [
                getaccountaddress,
                getaccount,
                getbalance,
                sendfrom
            ]
        },
        {twitcoin_wallet, [], [walletpassphrase]}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(ibrowse),
    Config.

end_per_suite(Config) ->
    {save_config, Config}.

init_per_testcase(_TestCase, Config) ->
    RpcUser = twitcoin:get_app_env(twitcoin_bitcoind_rpc_user, "bitcoinrpc"),
    RpcPassword =
        twitcoin:get_app_env(twitcoin_bitcoind_rpc_password, "password"),
    twitcoin_test_server:start(RpcUser, RpcPassword),
    Url = "http://127.0.0.1:"
              ++ integer_to_list(mochiweb_socket_server:get(testserver, port)),
    twitcoin_bitcoind_client:start(Url, {basic_auth, {RpcUser, RpcPassword}}),
    Config.

end_per_testcase(_TestCase, _Config) ->
    twitcoin_bitcoind_client:stop(),
    twitcoin_test_server:stop().

getaccountaddress(_) ->
    {ok, Addr1} = twitcoin_bitcoind_client:getaccountaddress("foo"),
    true = is_binary(Addr1),
    {ok, Addr1} = twitcoin_bitcoind_client:getaccountaddress(<<"foo">>),
    {ok, Addr2} = twitcoin_bitcoind_client:getaccountaddress("bar"),
    {ok, Addr2} = twitcoin_bitcoind_client:getaccountaddress(<<"bar">>),
    true = Addr1 =/= Addr2,
    {ok, Addr1} = twitcoin_bitcoind_client:getaccountaddress("foo").

getaccount(_) ->
    {ok, Addr1} = twitcoin_bitcoind_client:getaccountaddress("foo"),
    true = is_binary(Addr1),
    {ok, <<"foo">>} = twitcoin_bitcoind_client:getaccount(Addr1),
    {ok, <<"foo">>} = twitcoin_bitcoind_client:getaccount(?b2l(Addr1)),
    {error, Reason} = twitcoin_bitcoind_client:getaccount(<<"1234">>),
    true = is_binary(Reason).

getnewaddress(_) ->
    {ok, Addr1} = twitcoin_bitcoind_client:getnewaddress("foo"),
    true = is_binary(Addr1),
    {ok, Addr2} = twitcoin_bitcoind_client:getnewaddress(<<"foo">>),
    true = is_binary(Addr2),
    true = Addr2 =/= Addr1,
    {ok, Addr3} = twitcoin_bitcoind_client:getnewaddress("bar"),
    {ok, Addr3} = twitcoin_bitcoind_client:getnewaddress(<<"bar">>),
    true = is_binary(Addr3),
    true = Addr3 =/= Addr1,
    true = Addr3 =/= Addr2.

getbalance(_) ->
    {ok, 0.0} = twitcoin_bitcoind_client:getbalance("baz"),
    {ok, 0.0} = twitcoin_bitcoind_client:getbalance(<<"baz">>),
    {ok, 0.0} = twitcoin_bitcoind_client:getbalance("qux"),
    {ok, 0.0} = twitcoin_bitcoind_client:getbalance(<<"qux">>),
    {ok, 0.0} = twitcoin_bitcoind_client:getbalance("baz"),
    {ok, 0.0} = twitcoin_bitcoind_client:getbalance(<<"baz">>).

sendfrom(_) ->
    Account1 = "Ben Bitdiddle",
    Account2 = "Alyssa P. Hacker",
    {ok, Addr2} = twitcoin_bitcoind_client:getaccountaddress(Account2),
    {error, ?MSG_INSUFFICIENT_FUNDS} = twitcoin_bitcoind_client:sendfrom(Account1, Addr2, 0.25),
    twitcoin_test_server:set_balance(Account1, 1.0),
    {ok, 1.0} = twitcoin_bitcoind_client:getbalance(Account1),
    {ok, _} = twitcoin_bitcoind_client:sendfrom(?l2b(Account1), ?b2l(Addr2), 0.25),
    {ok, 0.25} = twitcoin_bitcoind_client:getbalance(Account2).

walletpassphrase(_) ->
    {error, _} = twitcoin_bitcoind_client:walletpassphrase("foo", 1),
    {ok, true} = twitcoin_bitcoind_client:walletpassphrase(twitcoin_test_server:test_wallet_passphrase(), 1).
