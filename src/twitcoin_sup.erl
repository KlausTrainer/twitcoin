%% @doc Supervisor for the twitcoin application.
-module(twitcoin_sup).

-behaviour(supervisor).

%% supervisor callbacks
-export([init/1]).

-export([start_link/0]).

-include("twitcoin.hrl").


%% External API

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% supervisor callback

init([]) ->
    Processes = [
        {
            twitcoin_db,
            {twitcoin_db, start, db_start_args()},
            permanent, 2000, worker, dynamic
        },
        {
            twitcoin_bitcoind_client,
            {twitcoin_bitcoind_client, start, bitcoind_client_start_args()},
            permanent, 2000, worker, dynamic
        },
        {
            twitcoin_web_auth_server,
            {twitcoin_web_auth_server, start, web_auth_server_start_args()},
            permanent, 2000, worker, dynamic
        },
        {
            twitcoin_twitter_client,
            {twitcoin_twitter_client, start, twitter_client_start_args()},
            permanent, 2000, worker, dynamic
        },
        {
            twitcoin_web,
            {twitcoin_web, start, web_start_args()},
            permanent, 2000, worker, dynamic
        }
    ],
    {ok, {{one_for_one, 1024, 8}, Processes}}.


%% Internal API

db_start_args() ->
    [twitcoin:get_app_env(twitcoin_db_path,
        twitcoin_deps:local_path(["priv", "twitcoin_db"]) ++ ".bitcask")].

bitcoind_client_start_args() ->
    Url = twitcoin:get_app_env(twitcoin_bitcoind_url, ?DEFAULT_BITCOIND_URL),
    RpcUser = twitcoin:get_app_env(twitcoin_bitcoind_rpc_user,
        ?DEFAULT_BITCOIND_RPC_USER),
    RpcPassword = twitcoin:get_app_env(twitcoin_bitcoind_rpc_password,
        ?DEFAULT_BITCOIND_RPC_PASSWORD),
    [Url, {basic_auth, {RpcUser, RpcPassword}}].

web_auth_server_start_args() ->
    SessionTimeout = twitcoin:get_app_env(twitcoin_session_timeout,
        ?DEFAULT_SESSION_TIMEOUT),
    MaxSessionCacheSize = twitcoin:get_app_env(twitcoin_session_cache_size,
        ?DEFAULT_MAX_SESSION_CACHE_SIZE),
    [SessionTimeout, MaxSessionCacheSize].

twitter_client_start_args() ->
    ConsumerKey = twitcoin:get_app_env(twitcoin_twitter_consumer_key),
    ConsumerSecret = twitcoin:get_app_env(twitcoin_twitter_consumer_secret),
    CallbackUrl = twitcoin:get_app_env(twitcoin_twitter_oauth_callback),
    [{ConsumerKey, ConsumerSecret, hmac_sha1}, CallbackUrl].

web_start_args() ->
    Ip = twitcoin:get_app_env(twitcoin_ip, ?DEFAULT_IP),
    Port = twitcoin:get_app_env(twitcoin_port, ?DEFAULT_PORT),
    SessionTimeout = twitcoin:get_app_env(twitcoin_session_timeout,
        ?DEFAULT_SESSION_TIMEOUT),
    MaxSessionCacheSize = twitcoin:get_app_env(twitcoin_session_cache_size,
        ?DEFAULT_MAX_SESSION_CACHE_SIZE),
    DocRoot = twitcoin:get_app_env(twitcoin_doc_root,
        twitcoin_deps:local_path(["priv", "www"])),
    TwitcoinAccount = twitcoin:get_app_env(twitcoin_account,
        ?DEFAULT_TWITCOIN_ACCOUNT),
    TwitcoinTxFee = twitcoin:get_app_env(twitcoin_tx_fee,
        ?DEFAULT_TWITCOIN_TX_FEE),
    [
        [
            {ip, Ip},
            {port, Port},
            {session_timeout, SessionTimeout},
            {max_session_cache_size, MaxSessionCacheSize},
            {doc_root, DocRoot},
            {twitcoin_account, TwitcoinAccount},
            {twitcoin_tx_fee, TwitcoinTxFee}
        ]
    ].
