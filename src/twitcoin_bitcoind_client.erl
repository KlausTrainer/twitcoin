%% @doc Talks to bitcoind via JSON-RPC.
-module(twitcoin_bitcoind_client).

-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_info/2, handle_cast/2,
    code_change/3, terminate/2
]).

-export([
    start/2,
    stop/0
]).

-export([
    getaccountaddress/1,
    getaccount/1,
    getnewaddress/1,
    getbalance/1,
    sendfrom/3,
    walletpassphrase/2
]).

-define(TIMEOUT, 32000).

-record(state, {
    bitcoind_url,
    bitcoind_basic_auth
}).

-include("twitcoin.hrl").


%% External API

-spec start(string(), {basic_auth, {string(), string()}}) ->
    {ok, pid()} | {error, term()}.
start(BitcoindUrl, BasicAuth) ->
    State = #state{
        bitcoind_url = BitcoindUrl,
        bitcoind_basic_auth = BasicAuth
    },
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop).


%% bitcoind functions

-spec getaccountaddress(string() | binary()) ->
    {ok, binary()} | {error, binary()}.
getaccountaddress(Account) ->
    gen_server:call(?MODULE, {"getaccountaddress", [Account]}, ?TIMEOUT).

-spec getaccount(string() | binary()) -> {ok, binary()} | {error, binary()}.
getaccount(Address) ->
    gen_server:call(?MODULE, {"getaccount", [Address]}, ?TIMEOUT).

-spec getnewaddress(string() | binary()) -> {ok, binary()} | {error, binary()}.
getnewaddress(Account) ->
    gen_server:call(?MODULE, {"getnewaddress", [Account]}, ?TIMEOUT).

-spec getbalance(string() | binary()) -> {ok, float()} | {error, binary()}.
getbalance(Account) ->
    gen_server:call(?MODULE, {"getbalance", [Account]}, ?TIMEOUT).

-spec sendfrom(string() | binary(), string() | binary(), float()) ->
    {ok, TxId :: binary()} | {error, binary()}.
sendfrom(SenderAccount, RecvAddr, Amount) ->
    case getbalance(SenderAccount) of
    {error, Reason} ->
        {error, Reason};
    {ok, Balance} when Balance < Amount ->
        {error, ?MSG_INSUFFICIENT_FUNDS};
    {ok, _Balance} ->
        gen_server:call(?MODULE, {"sendfrom", [SenderAccount, RecvAddr, Amount]}, ?TIMEOUT)
    end.

-spec walletpassphrase(string() | binary(), integer()) ->
    {ok, true} | {error, binary()}.
walletpassphrase(Passphrase, Timeout) ->
    gen_server:call(?MODULE, {"walletpassphrase", [Passphrase, Timeout]}, ?TIMEOUT).


%% gen_server callbacks

init(State) ->
    {ok, State}.

handle_call({"walletpassphrase", Params}, _From,
    #state{bitcoind_url = Url, bitcoind_basic_auth = BasicAuth} = State) ->
    case call("walletpassphrase", Url, BasicAuth, Params) of
    {error, Reason} ->
       error_logger:error_msg("~p:walletpassphrase/2: ~p~n",
                              [?MODULE, {error, Reason}]),
       {reply, {error, ?MSG_BITCOIND_ERROR}, State};
    {ok, "500", _, Json} ->
        {reply, {error, error_message(Json)}, State};
    {ok, "200", _, _} ->
        {reply, {ok, true}, State}
    end;

handle_call({Method, Params}, _From,
    #state{bitcoind_url = Url, bitcoind_basic_auth = BasicAuth} = State) ->
    case call(Method, Url, BasicAuth, Params) of
    {error, Reason} ->
       error_logger:error_msg("~p:handle_call/3: ~p, Method: ~p, Params: ~p~n",
                              [?MODULE, {error, Reason}, Method, Params]),
       {reply, {error, ?MSG_BITCOIND_ERROR}, State};
    {ok, "500", _, Json} ->
        {reply, {error, error_message(Json)}, State};
    {ok, "200", _, Json} ->
        {reply, {ok, result(Json)}, State}
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

-type rpc_params() :: [rpc_param()].
-type rpc_param() :: binary() | string() | float() | integer().

-type ibrowse_response() :: {ok, string(), [{string(), string()}], binary()}
                            | {error, term()}.
-spec call(string(), string(), {basic_auth, {string(), string()}}, rpc_params()) -> ibrowse_response().
call(Method, Url, BasicAuth, Params) ->
    MethodBin = ?l2b(Method),
    ParamsBin = params(Params),
    Req = <<"{\"method\":\"", MethodBin/binary, "\",\"params\":[",
            ParamsBin/binary,
            "],\"id\":\"", MethodBin/binary, "\"}">>,
    ibrowse:send_req(Url, [BasicAuth], post, Req, ?IBROWSE_OPTS).

-spec params(rpc_params()) -> binary().
params(Params) ->
    ?l2b(string:join([param(P) || P <- Params], ",")).

-spec param(rpc_param()) -> string().
param(Arg) when is_binary(Arg) ->
    "\"" ++ ?b2l(Arg) ++ "\"";
param(Arg) when is_list(Arg) ->
    "\"" ++ Arg ++ "\"";
param(Arg) when is_float(Arg) ->
    ?f2l(Arg);
param(Arg) when is_integer(Arg) ->
    ?i2l(Arg).

result(JsonBin) ->
    {JsonObj} = jiffy:decode(JsonBin),
    proplists:get_value(<<"result">>, JsonObj).

error_message(JsonBin) ->
    {JsonObj} = jiffy:decode(JsonBin),
    {Error} = proplists:get_value(<<"error">>, JsonObj),
    proplists:get_value(<<"message">>, Error).
