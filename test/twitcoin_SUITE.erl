-module(twitcoin_SUITE).

-compile(export_all).

-import(test_utils, [temp_file_name/0]).

-include_lib("common_test/include/ct.hrl").

-include("twitcoin.hrl").


all() ->
    [{group, twitcoin_api}].

groups() ->
	[
        {twitcoin_api, [shuffle], [get_address, send_from]}
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
    twitcoin_db:start(temp_file_name()),
    Config.

end_per_testcase(_TestCase, _Config) ->
    twitcoin_bitcoind_client:stop(),
    twitcoin_db:stop(),
    twitcoin_test_server:stop().

get_address(_) ->
    Account1 = "Alyssa P. Hacker",
    Account2 = "Ben Bitdiddle",
    {ok, Addr1} = twitcoin:get_address(Account1),
    {ok, Addr1} = twitcoin_db:get_address(Account1),
    {ok, Addr1} = twitcoin:get_address(Account1),
    {ok, Addr2} = twitcoin_bitcoind_client:getaccountaddress(Account2),
    twitcoin_db:put_address(Account2, Addr2),
    {ok, Addr2} = twitcoin:get_address(Account2),
    {ok, Addr2} = twitcoin_db:get_address(Account2),
    {ok, Addr2} = twitcoin:get_address(Account2),
    true = Addr1 =/= Addr2.

send_from(_) ->
    Account1 = "Alyssa P. Hacker",
    Account2 = "Ben Bitdiddle",
    TxFee = 0.01,
    {ok, Addr1} = twitcoin_bitcoind_client:getaccountaddress(Account1),
    {ok, Addr2} = twitcoin_bitcoind_client:getaccountaddress(Account2),
    twitcoin_db:put_address(Account1, Addr1),
    twitcoin_db:put_address(Account2, Addr2),
    {error, ?MSG_INSUFFICIENT_FUNDS} = twitcoin:send_from(Account1, Addr2, 0.25, TxFee),
    twitcoin_test_server:set_balance(Account1, 1.0),
    {error, ?MSG_INSUFFICIENT_FUNDS} = twitcoin:send_from(Account1, Addr2, 1.25, TxFee),
    {error, ?MSG_INSUFFICIENT_FUNDS} = twitcoin:send_from(Account1, Addr2, 1.0, TxFee),
    {ok, Addr1} = twitcoin_db:get_address(Account1),
    {ok, Addr2} = twitcoin_db:get_address(Account2),
    {ok, TxId0} = twitcoin:send_from(Account1, Addr2, 0.25, TxFee),
    true = is_binary(TxId0),
    {ok, Addr1} = twitcoin_db:get_address(Account1),
    {ok, Addr3} = twitcoin_db:get_address(Account2),
    true = Addr3 =/= Addr2,
    {ok, TxId1} = twitcoin:send_from(Account2, ?b2l(Addr1), 0.24, TxFee),
    true = is_binary(TxId1),
    {ok, Addr4} = twitcoin_db:get_address(Account2),
    true = Addr4 =/= Addr1.
