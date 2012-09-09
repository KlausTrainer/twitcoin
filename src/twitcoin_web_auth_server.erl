%% @doc http(s) authentication server for twitcoin.
-module(twitcoin_web_auth_server).

-export([start/2, stop/0]).
-export([
    create_session/3,
    session/1,
    session/2,
    delete_session/1
]).

-import(twitcoin_util, [binary_to_hexstring/1, hexstring_to_binary/1]).

-define(CACHE, twitcoin_web_auth_cache).

-include("twitcoin.hrl").


%% External API

-spec start(pos_integer(), pos_integer()) -> {ok, pid()}.
start(SessionTimeout, MaxSessionCacheSize) ->
    CacheOpts = [
       {name, ?CACHE},
       {ttl, SessionTimeout},
       {size, MaxSessionCacheSize}
    ],
    term_cache_ets:start_link(CacheOpts).

stop() ->
    term_cache_ets:stop(?CACHE).

-spec create_session(string(), string(), string()) ->
    AuthSessionCookie :: string().
create_session(Account, Token, TokenSecret) ->
    Account2 = ?l2b(Account),
    term_cache_ets:put(?CACHE, Account2, {?l2b(Token), ?l2b(TokenSecret)}),
    ShaMac = crypto:sha_mac(TokenSecret, Account2),
    binary_to_hexstring(<<Account2/binary,":",ShaMac/binary>>).

-spec delete_session(string()) -> ok.
delete_session(Account) ->
    term_cache_ets:delete(?CACHE, ?l2b(Account)).

-spec session(undefined | string()) ->
    not_found
    | nok
    | {ok, {Account :: string(), Token :: string(), TokenSecret :: string()}}.
session(AuthSessionCookie) ->
    case AuthSessionCookie of
    undefined ->
        not_found;
    [] ->
        not_found;
    _ ->
        Account = hd(
            string:tokens(?b2l(hexstring_to_binary(AuthSessionCookie)), ":")),
        session(Account, AuthSessionCookie)
    end.

-spec session(string() | binary(), undefined | string()) ->
    not_found
    | nok
    | {ok, {Account :: string(), Token :: string(), TokenSecret :: string()}}.
session(Account, AuthSessionCookie) ->
    case AuthSessionCookie of
    undefined ->
        not_found;
    [] ->
        not_found;
    _ ->
        AccountBinary = case is_binary(Account) of
        true -> Account;
        false -> ?l2b(Account)
        end,
        case term_cache_ets:get(?CACHE, AccountBinary) of
        not_found ->
            not_found;
        {ok, {Token, TokenSecret}} ->
            ShaMac = crypto:sha_mac(TokenSecret, AccountBinary),
            case hexstring_to_binary(AuthSessionCookie)
                =:= <<AccountBinary/binary,":",ShaMac/binary>> of
            false ->
                nok;
            true ->
                {ok, {Account, ?b2l(Token), ?b2l(TokenSecret)}}
            end
        end
    end.
