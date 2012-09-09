%% @doc Handles storage of user account data.
-module(twitcoin_db).

-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1, handle_call/3, handle_info/2, handle_cast/2,
    code_change/3, terminate/2
]).

-export([start/1, stop/0]).
-export([get_address/1, delete_address/1, put_address/2]).

-include("twitcoin.hrl").


%% External API

-spec start(string()) -> {ok, pid()} | {error, term()}.
start(DbPath) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [DbPath], []).

-spec stop() -> ok.
stop() ->
    catch gen_server:call(?MODULE, stop),
    ok.

-spec get_address(string() | binary()) -> not_found | {ok, binary()}.
get_address(Account) ->
    gen_server:call(?MODULE, {get_address, binary(Account)}, infinity).

-spec delete_address(string() | binary()) -> ok | {error, term()}.
delete_address(Account) ->
    gen_server:call(?MODULE, {delete_address, binary(Account)}, infinity).

-spec put_address(string() | binary(), string() | binary()) ->
    ok | {error, term()}.
put_address(Account, Address) ->
    gen_server:call(?MODULE, {put_address, binary(Account), binary(Address)}, infinity).


%% gen_server callbacks

init([DbPath]) ->
    {ok, bitcask:open(DbPath, [read_write, sync_on_put])}.


handle_call({get_address, Account}, _From, State) ->
    {reply, bitcask:get(State, Account), State};

handle_call({delete_address, Account}, _From, State) ->
    {reply, bitcask:delete(State, Account), State};

handle_call({put_address, Account, Address}, _From, State) ->
    {reply, bitcask:put(State, Account, Address), State};

handle_call(stop, _From, State) ->
    bitcask:close(State),
    {stop, normal, ok, nil}.


handle_cast(_Req, State) ->
    {noreply, State}.


handle_info(timeout, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, State) ->
    bitcask:close(State).


%% Internal API

binary(Arg) when is_binary(Arg) ->
    Arg;
binary(Arg) when is_list(Arg) ->
    ?l2b(Arg).
