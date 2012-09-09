-module(test_utils).

-export([temp_file_name/0]).

temp_file_name() ->
    {A, B, C} = now(),
    "/tmp/twitcoin_test_db_" ++ lists:flatten(io_lib:format("~p~p~p", [A, B, C])).
