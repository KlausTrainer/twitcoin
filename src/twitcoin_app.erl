%% @doc Callbacks for the twitcoin application.
-module(twitcoin_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

-export([set_config/0, set_config/1]).


%% External API

%% @doc application start callback for twitcoin.
-spec start(_Type, _StartArgs) -> {ok, pid()}.
start(_Type, _StartArgs) ->
    set_config(),
    {ok, _Pid} = twitcoin_sup:start_link().

%% @doc application stop callback for twitcoin.
-spec stop(_State) -> ok.
stop(_State) ->
    ok.

%% @doc Read the twitcoin configuration file and set environment variables.
-spec set_config() -> ok | {error, term()}.
set_config() ->
    DefaultConfigPath = twitcoin_deps:local_path(["config"])
                            ++ "/twitcoin.config",
    set_config(twitcoin:get_app_env(configpath, DefaultConfigPath)).

%% @doc Read the twitcoin configuration file with filename `ConfigPath` and
%%      set environment variables.
-spec set_config(list()) -> ok | {error, term()}.
set_config(ConfigPath) ->
    case file:consult(ConfigPath) of
    {ok, Terms} ->
        set_erlenv(Terms);
    {error, Reason} ->
        error_logger:warning_msg(
            "Failed to read configuration from: ~p (~p)~n",
            [ConfigPath, Reason]),
        {error, Reason}
    end.


%% Internal API

set_erlenv([]) ->
    ok;
set_erlenv([{K, V} | T]) ->
    application:set_env(twitcoin, K, V),
    error_logger:info_msg("set env variable ~p: ~p~n", [K, V]),
    set_erlenv(T).
