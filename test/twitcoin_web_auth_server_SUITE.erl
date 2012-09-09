-module(twitcoin_web_auth_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, twitcoin_web_auth_server}].

groups() ->
	[
        {
            twitcoin_web_auth_server, [],
                [
                    create_session,
                    session,
                    delete_session
                ]
        }
    ].

init_per_suite(Config) ->
    application:start(crypto),
    Config.

end_per_suite(Config) ->
    {save_config, Config}.

init_per_testcase(_TestCase, Config) ->
    {ok, Cache} = twitcoin_web_auth_server:start(60000, "1MB"),
    [{cache, Cache} | proplists:delete(cache, Config)].

end_per_testcase(_TestCase, _Config) ->
    twitcoin_web_auth_server:stop().

create_session(Config) ->
    Cache = proplists:get_value(cache, Config),
    Account1 = "Fooman",
    Account1Binary = <<"Fooman">>,
    CookieAuthHeader = twitcoin_web_auth_server:create_session(Account1,
        "token", "token secret"),
    {ok, {_Token, TokenSecret}} = term_cache_ets:get(Cache, Account1Binary),
    Cookie = twitcoin_util:hexstring_to_binary(CookieAuthHeader),
    ShaMac = crypto:sha_mac(TokenSecret, Account1Binary),
    Cookie = <<Account1Binary/binary,":",ShaMac/binary>>.

session(_) ->
    Account1 = "Fooman",
    Account2 = "Batman",
    not_found = twitcoin_web_auth_server:session(Account1, undefined),
    not_found = twitcoin_web_auth_server:session(Account1, ""),
    CookieAuthHeader = twitcoin_web_auth_server:create_session(Account1,
        "token", "token secret"),
    {ok, {"Fooman", "token", "token secret"}} =
        twitcoin_web_auth_server:session(CookieAuthHeader),
    {ok, {"Fooman", "token", "token secret"}} =
        twitcoin_web_auth_server:session(Account1, CookieAuthHeader),
    nok = twitcoin_web_auth_server:session(Account1, "DEADBEEF"),
    not_found = twitcoin_web_auth_server:session(Account2, CookieAuthHeader).

delete_session(_) ->
    Account1 = "Fooman",
    CookieAuthHeader = twitcoin_web_auth_server:create_session(Account1,
        "token", "token secret"),
    {ok, {"Fooman", "token", "token secret"}} =
        twitcoin_web_auth_server:session(Account1, CookieAuthHeader),
    ok = twitcoin_web_auth_server:delete_session(Account1),
    not_found = twitcoin_web_auth_server:session(Account1, CookieAuthHeader).
