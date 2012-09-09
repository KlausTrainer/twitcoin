%% @doc Web server for twitcoin.
-module(twitcoin_web).

-export([start/1, stop/0, loop/6]).

-define(HEADERS, [{"Content-Type", "application/json; charset=utf-8"}]).
-define(ACCOUNT_DATA_CACHE_TTL, 240000). % four minutes

-include("twitcoin.hrl").


%% External API

start(Options) ->
    {SessionTimeout, Options1} = get_option(session_timeout, Options),
    {MaxAccountDataCacheSize, Options2} =
        get_option(max_session_cache_size, Options1),
    {DocRoot, Options3} = get_option(doc_root, Options2),
    {TwitcoinAccount, Options4} = get_option(twitcoin_account, Options3),
    {TwitcoinTxFee, Options5} = get_option(twitcoin_tx_fee, Options4),
    CacheOpts = [
       {ttl, SessionTimeout},
       {size, MaxAccountDataCacheSize}
    ],
    {ok, AccountDataCache} = term_cache_ets:start_link(CacheOpts),
    Loop = fun (Req) ->
               try
                   ?MODULE:loop(
                       Req,
                       DocRoot,
                       AccountDataCache,
                       SessionTimeout,
                       TwitcoinAccount,
                       TwitcoinTxFee)
               catch
               Type:What ->
                   Report = ["web request failed", {path, Req:get(raw_path)},
                             {type, Type}, {what, What},
                             {trace, erlang:get_stacktrace()}],
                   error_logger:error_report(Report),
                   Body = <<"{\"result\":null,\"error\":\"internal server error\"}">>,
                   Req:respond({500, ?HEADERS, Body})
               end
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options5]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot, AccountDataCache, SessionTimeout, TwitcoinAccount,
    TwitcoinTxFee) ->
    Method = Req:get(method),
    Cookie = Req:get_cookie_value("AuthSession"),
    case string:tokens(Req:get(path), "/") of
    [] ->
        AccountParam = proplists:get_value("account", Req:parse_qs()),
        case twitcoin_web_auth_server:session(Cookie) of
        nok ->
            do_401(Req);
        {ok, {AccountParam, _, _}} ->
            Req:serve_file("account.html", DocRoot);
        {ok, {Account, _, _}} ->
            twitcoin_web_auth_server:delete_session(Account),
            Req:serve_file("index.html", DocRoot);
        not_found ->
            Req:serve_file("index.html", DocRoot)
        end;
    ["api", "session"] when Method =:= 'GET' ->
        case twitcoin_twitter_client:get_request_token() of
        {error, _Reason} ->
            do_200(Req, {error, ?MSG_TWITTER_ERROR});
        {ok, Token} ->
            redirect_to(Req, twitcoin_twitter_client:authenticate_url(Token))
        end;
    ["api", "session"] when Method =:= 'DELETE' ->
        case twitcoin_web_auth_server:session(Cookie) of
        nok ->
            do_401(Req);
        not_found ->
            do_200(Req, {ok, true});
        {ok, {Account, _, _}} ->
            twitcoin_web_auth_server:delete_session(Account),
            do_200(Req, {ok, true})
        end;
    ["api", "twitter_sign_in"] when Method =:= 'GET' ->
        Params = Req:parse_qs(),
        Denied = proplists:get_value("denied", Params),
        Token = proplists:get_value("oauth_token", Params),
        Verifier = proplists:get_value("oauth_verifier", Params),
        if
        Denied =/= undefined ->
            redirect_to(Req, absolute_path(Req, "/"));
        Token =:= undefined; Verifier =:= undefined ->
            do_400(Req, ?MSG_MISSING_QUERY_PARAMETERS);
        true ->
            case twitcoin_twitter_client:get_access_token(Verifier, Token) of
            {error, _Reason} ->
                do_200(Req, {error, ?MSG_TWITTER_ERROR});
            nok ->
                do_401(Req);
            {ok, Account, Token2, TokenSecret2} ->
                spawn(
                    %% While clients are being redirected, try to get their
                    %% account data and, if available, cache it, so that the
                    %% data is already right there when they are requesting it.
                    fun() ->
                        get_account(AccountDataCache, Token2, TokenSecret2,
                            Account)
                    end
                ),
                NewCookie = twitcoin_web_auth_server:create_session(Account,
                    Token2, TokenSecret2),
                Ts = lists:flatten([?i2l(I) || I <- ?t2l(os:timestamp())]),
                Url = "/?account=" ++ Account ++ "&ts=" ++ Ts,
                RedirectUrl = absolute_path(Req, Url),
                CookieOptions = [
                    {path, "/"},
                    {http_only, true},
                    {secure, true},
                    {max_age, SessionTimeout div 1000} % in seconds
                ],
                ResponseHeaders = [
                    mochiweb_cookies:cookie("AuthSession", NewCookie,
                        CookieOptions)
                ],
                redirect_to(Req, RedirectUrl, ResponseHeaders)
            end
        end;
    ["api", "account", Account1] when Method =:= 'GET' ->
        case twitcoin_web_auth_server:session(Cookie) of
        not_found ->
            redirect_to(Req, absolute_path(Req, "/"));
        nok ->
            do_401(Req);
        {ok, {Account2, Token, TokenSecret}} ->
            case get_account(AccountDataCache, Token, TokenSecret, Account1) of
            {ok, AccountData} when Account1 =/= Account2 ->
                %% Remove address and balance; they're private!
                AccountData1 = proplists:delete(address, AccountData),
                AccountData2 = proplists:delete(balance, AccountData1),
                %%
                do_200(Req, {ok, AccountData2});
            Else ->
                do_200(Req, Else)
            end
        end;
    ["api", "send"] when Method =:= 'POST' ->
        try
            {JsonObj} = jiffy:decode(Req:recv_body()),
            SenderAccount = proplists:get_value(<<"sender_account">>, JsonObj),
            Receiver = proplists:get_value(<<"receiver">>, JsonObj),
            Amount = float(proplists:get_value(<<"amount">>, JsonObj)),
            case twitcoin_web_auth_server:session(SenderAccount, Cookie) of
            not_found ->
                redirect_to(Req, absolute_path(Req, "/"));
            nok ->
                do_401(Req);
            {ok, {SenderAccount, Token, TokenSecret}} ->
                Result = case Receiver of
                <<"@",Receiver1/binary>> ->
                    case twitcoin:get_account(Token, TokenSecret, Receiver1) of
                    {error, Reason} ->
                        {error, Reason};
                    {ok, AccountData} ->
                        Address = proplists:get_value(address, AccountData),
                        send_from(SenderAccount, Address, Amount,
                            TwitcoinAccount, TwitcoinTxFee)
                    end;
                _ ->
                    send_from(SenderAccount, Receiver, Amount,
                        TwitcoinAccount, TwitcoinTxFee)
                end,
                %% We want to make sure that we don't keep
                %% any stale account data in the cache.
                term_cache_ets:delete(AccountDataCache, SenderAccount),
                do_200(Req, Result)
            end
        catch _:_ ->
            do_400(Req, ?MSG_INVALID_JSON)
        end;
    _ when Method =:= 'GET'; Method =:= 'PUT'; Method =:= 'DELETE';
        Method =:= 'POST' ->
        do_404(Req);
    _ ->
        do_405(Req)
    end.


%% Internal API

%% @doc Wraps a caching layer around `twitcoin:get_account/3`.
-spec get_account(pid(), string(), string(), string()) ->
    {ok, account_data()} | {error, binary()}.
get_account(AccountDataCache, Token, TokenSecret, Account) ->
    Account0 = ?l2b(Account),
    case term_cache_ets:get(AccountDataCache, Account0) of
    not_found ->
        case twitcoin:get_account(Token, TokenSecret, Account) of
        {ok, AccountData} ->
            term_cache_ets:put(AccountDataCache, Account0, AccountData),
            % delete cached account data items after `?ACCOUNT_DATA_CACHE_TTL`
            % independently of whether and when they've been accessed
            timer:apply_after(?ACCOUNT_DATA_CACHE_TTL, term_cache_ets, delete,
                [AccountDataCache, Account0]),
            {ok, AccountData};
        Else ->
            Else
        end;
    {ok, AccountData} ->
        {ok, AccountData}
    end.

%% @doc Wraps `twitcoin:send_from/4`. For each transaction, it sends half of
%%      the transaction fee to the Twitcoin Twitter account. The other half of
%%      the fee is used for the network. That is, a quarter of
%%      `twitcoin_tx_fee` is used as a fee for the actual transaction initiated
%%      by the user, while the other quarter is used for the transaction to the
%%      `twitcoin_account`.
-spec send_from(string() | binary(), string() | binary(), float(), string(), float()) ->
    {ok, TxId :: binary()} | {error, binary()}.
send_from(SenderAccount, ReceiverAddress, Amount, TwitcoinAccount,
    TwitcoinTxFee) ->
    case twitcoin:send_from(SenderAccount, ReceiverAddress, Amount,
        TwitcoinTxFee / 4.0) of
    {ok, TxId} ->
        spawn(
            fun() ->
                {ok, TwitcoinAccountAddress} =
                    twitcoin:get_address(TwitcoinAccount),
                twitcoin:send_from(SenderAccount, TwitcoinAccountAddress,
                    TwitcoinTxFee / 2.0, TwitcoinTxFee / 4.0)
            end),
        {ok, TxId};
    Else ->
        Else
    end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

absolute_path(Req, Path) ->
    ?a2l(Req:get(scheme)) ++ "://" ++ Req:get_header_value(host) ++ Path.

redirect_to(Req, Url) ->
    redirect_to(Req, Url, []).

redirect_to(Req, Url, Headers) ->
    ResponseHeaders = [
        {"Location", Url},
        {"Content-Type", "text/html"}
        | Headers
    ],
    RedirectMsg = "You are being redirected to <a href=\"" ++ Url ++ "\">"
        ++ Url ++ "</a>.",
    Req:respond({302, ResponseHeaders, RedirectMsg}).

do_200(Req, {ok, Result}) when is_atom(Result) ->
    Result2 = ?l2b(?a2l(Result)),
    Response = <<"{\"result\":",Result2/binary,",\"error\":null}">>,
    Req:respond({200, ?HEADERS, Response});
do_200(Req, {ok, Result}) when is_binary(Result) ->
    Response = <<"{\"result\":\"",Result/binary,"\",\"error\":null}">>,
    Req:respond({200, ?HEADERS, Response});
do_200(Req, {ok, AccountData}) when is_list(AccountData) ->
    Response = jiffy:encode({[{result, {AccountData}}, {error, null}]}),
    Req:respond({200, ?HEADERS, Response});
do_200(Req, {error, Msg}) ->
    Response = <<"{\"result\":null,\"error\":\"",Msg/binary,"\"}">>,
    Req:respond({200, ?HEADERS, Response}).

do_400(Req, Reason) ->
    Body = <<"{\"result\":null,\"error\":\"",Reason/binary,"\"}">>,
    Req:respond({400, ?HEADERS, Body}).

do_401(Req) ->
    Body = <<"{\"result\":null,\"error\":\"Invalid credentials.\"}">>,
    Req:respond({401, ?HEADERS, Body}).

do_404(Req) ->
    Body = <<"{\"result\":null,\"error\":\"What the hell are you here looking for?\"}">>,
    Req:respond({404, ?HEADERS, Body}).

do_405(Req) ->
    Body = <<"{\"result\":null,\"error\":\"This request method is not allowed.\"}">>,
    Req:respond({405, ?HEADERS, Body}).
