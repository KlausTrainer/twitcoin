-module(twitcoin_util).

-export([binary_to_hexstring/1, hexstring_to_binary/1]).

-include("twitcoin.hrl").

binary_to_hexstring(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- ?b2l(Bin)]).

hexstring_to_binary(S) ->
  hexstring_to_binary(S, []).

hexstring_to_binary([], Acc) ->
  ?l2b(lists:reverse(Acc));
hexstring_to_binary([X, Y | T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X, Y]),
  hexstring_to_binary(T, [V | Acc]).
