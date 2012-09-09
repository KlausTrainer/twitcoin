-module(twitcoin_db_SUITE).

-compile(export_all).

-import(test_utils, [temp_file_name/0]).

-include_lib("common_test/include/ct.hrl").

-include("twitcoin.hrl").


all() ->
    [{group, twitcoin_db}].

groups() ->
	[{twitcoin_db, [], [put_get_delete_address]}].

init_per_testcase(_TestCase, Config) ->
    twitcoin_db:start(temp_file_name()),
    Config.

end_per_testcase(_TestCase, _Config) ->
    twitcoin_db:stop().

put_get_delete_address(_) ->
    Account1 = <<"Alyssa P. Hacker">>,
    Account2 = <<"Ben Bitdiddle">>,
    not_found = twitcoin_db:get_address(Account1),
    ok = twitcoin_db:put_address(Account2, <<"1234">>),
    ok = twitcoin_db:put_address(Account2, "1234"),
    not_found = twitcoin_db:get_address(Account1),
    {ok, <<"1234">>} = twitcoin_db:get_address(Account2),
    {ok, <<"1234">>} = twitcoin_db:get_address(?b2l(Account2)),
    ok = twitcoin_db:put_address(Account1, <<"5678">>),
    ok = twitcoin_db:put_address(Account1, "5678"),
    {ok, <<"5678">>} = twitcoin_db:get_address(Account1),
    {ok, <<"5678">>} = twitcoin_db:get_address(?b2l(Account1)),
    {ok, <<"1234">>} = twitcoin_db:get_address(Account2),
    ok = twitcoin_db:put_address(Account1, "8765"),
    ok = twitcoin_db:put_address(Account1, <<"8765">>),
    {ok, <<"8765">>} = twitcoin_db:get_address(?b2l(Account1)),
    {ok, <<"8765">>} = twitcoin_db:get_address(Account1),
    {ok, <<"1234">>} = twitcoin_db:get_address(Account2),
    ok = twitcoin_db:delete_address(Account1),
    ok = twitcoin_db:delete_address(?b2l(Account2)),
    not_found = twitcoin_db:get_address(Account1),
    not_found = twitcoin_db:get_address(Account2),
    not_found = twitcoin_db:get_address(?b2l(Account2)).
