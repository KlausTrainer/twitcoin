%% @doc Talks to Twitter.
-module(twitcoin_twitter_client).

-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_info/2, handle_cast/2,
    code_change/3, terminate/2
]).

-export([
    start/2,
    stop/0,
    get_request_token/0, authenticate_url/1, get_access_token/2,
    get_user_data/3
]).

-define(OAUTH_TOKEN_TTL, 480000). % eight minutes
-define(TIMEOUT, 32000).

-type consumer() :: {ConsumerKey :: string(), ConsumerSecret :: string(), hmac_sha1}.

-record(state, {
    consumer,
    twitcoin_twitter_oauth_callback,
    oauth_token_cache
}).

-include("twitcoin.hrl").


%% External API

-spec start(consumer(), string()) -> {ok, pid()} | {error, term()}.
start(Consumer = {_ConsumerKey, _ConsumerSecret, hmac_sha1}, CallbackUrl) ->
    {ok, Cache} = term_cache_ets:start_link([{ttl, ?OAUTH_TOKEN_TTL}]),
    State = #state{
        consumer = Consumer,
        twitcoin_twitter_oauth_callback = CallbackUrl,
        oauth_token_cache = Cache
    },
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).


%% Twitter API functions

-spec get_request_token() -> {ok, Token :: string()} | {error, term()}.
get_request_token() ->
    Url = "https://api.twitter.com/oauth/request_token",
    gen_server:call(?MODULE, {get_request_token, Url}, ?TIMEOUT).

-spec authenticate_url(string()) -> string().
authenticate_url(Token) ->
    Url = "https://api.twitter.com/oauth/authenticate",
    oauth:uri(Url, [{"oauth_token", Token}]).

-spec get_access_token(string(), string()) ->
    nok
    | {ok, ScreenName :: string(), Token :: string(), TokenSecret :: string()}
    | {error, term()}.
get_access_token(Verifier, Token) ->
    Url = "https://api.twitter.com/oauth/access_token",
    gen_server:call(?MODULE, {get_access_token, Url, Verifier, Token}, ?TIMEOUT).

-spec get_user_data(string(), string(), string()) ->
    not_found
    | {error, term()}
    | {ok, {Name :: binary(), ProfileImgUrl :: binary()}}.
get_user_data(Token, TokenSecret, ScreenName) ->
    Url = "http://api.twitter.com/1/users/show.json",
    Params = [{"screen_name", ScreenName}],
    gen_server:call(?MODULE, {get_user_data, Url, Params, Token, TokenSecret}, ?TIMEOUT).


%% gen_server callbacks

init(Consumer) ->
    {ok, Consumer}.


handle_call({get_request_token, Url}, _From, State) ->
    Consumer = State#state.consumer,
    CallbackUrl = State#state.twitcoin_twitter_oauth_callback,
    Params = [{"oauth_callback", CallbackUrl}],
    case oauth:get(Url, Params, Consumer, "", "", ?IBROWSE_OPTS) of
    {ok, "200", _, Body} ->
        ResponseParams = mochiweb_util:parse_qs(Body),
        Token = proplists:get_value("oauth_token", ResponseParams),
        TokenSecret = proplists:get_value("oauth_token_secret", ResponseParams),
        term_cache_ets:put(State#state.oauth_token_cache, Token, TokenSecret),
        {reply, {ok, Token}, State};
    {ok, _Response} ->
        {reply, {error, <<"Error when requesting OAuth token.">>, State}};
    {error, Reason} ->
        {reply, {error, Reason}, State}
    end;

handle_call({get_access_token, Url, Verifier, Token}, _From, State) ->
    Params = [{"oauth_verifier", Verifier}],
    case term_cache_ets:get(State#state.oauth_token_cache, Token) of
    not_found ->
        {reply, nok, State};
    {ok, TokenSecret} ->
        case oauth:get(Url, Params, State#state.consumer, Token, TokenSecret, ?IBROWSE_OPTS) of
        {ok, "200", _, Body} ->
            ResponseParams = mochiweb_util:parse_qs(Body),
            ScreenName = proplists:get_value("screen_name", ResponseParams),
            Token2 = proplists:get_value("oauth_token", ResponseParams),
            TokenSecret2 = proplists:get_value("oauth_token_secret", ResponseParams),
            {reply, {ok, ScreenName, Token2, TokenSecret2}, State};
        {ok, _, _, _} ->
            {reply, nok, State};
        Error ->
            {reply, Error, State}
        end
    end;

handle_call({get_user_data, Url, Params, Token, TokenSecret}, _From, State) ->
    case oauth:get(Url, Params, State#state.consumer, Token, TokenSecret, ?IBROWSE_OPTS) of
    {error, _Reason} ->
       {reply, {error, ?MSG_TWITTER_ERROR}, State};
    {ok, "200", _, ResponseBody} ->
        {reply, {ok, parse_user_data(ResponseBody)}, State};
    {ok, _, _, _} ->
        {reply, not_found, State}
    end.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(timeout, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.


%% Internal API

parse_user_data(JsonBin) ->
    {JsonObj} = jiffy:decode(JsonBin),
    Name = proplists:get_value(<<"name">>, JsonObj),
    ProfileImgUrl = proplists:get_value(<<"profile_image_url_https">>, JsonObj),
    {Name, ProfileImgUrl}.
