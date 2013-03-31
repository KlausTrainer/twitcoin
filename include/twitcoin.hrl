-define(l2b(V), list_to_binary(V)).
-define(l2f(V), list_to_float(V)).
-define(l2i(V), list_to_integer(V)).
-define(a2l(V), atom_to_list(V)).
-define(b2l(V), binary_to_list(V)).
-define(f2l(V), float_to_list(V)).
-define(i2l(V), integer_to_list(V)).
-define(t2l(V), tuple_to_list(V)).

-define(DEFAULT_BITCOIND_URL, "http://127.0.0.1:8332").
-define(DEFAULT_BITCOIND_RPC_USER, "bitcoinrpc").
-define(DEFAULT_BITCOIND_RPC_PASSWORD, "password").
-define(DEFAULT_TWITCOIN_TX_FEE, 0.001).
-define(DEFAULT_TWITCOIN_ACCOUNT, "twitcoin_net").
-define(DEFAULT_IP, {127,0,0,1}).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_SESSION_TIMEOUT, 3600000). % milliseconds
-define(DEFAULT_MAX_SESSION_CACHE_SIZE, "256MB").

-define(MSG_INVALID_JSON, <<"Invalid JSON.">>).
-define(MSG_MISSING_QUERY_PARAMETERS, <<"Missing request parameters.">>).
-define(MSG_BITCOIND_ERROR, <<"Cannot talk to bitcoind.">>).
-define(MSG_TWITTER_ERROR, <<"Cannot talk to Twitter API.">>).
-define(MSG_INSUFFICIENT_FUNDS, <<"Insufficient funds.">>).
-define(MSG_NO_TWITTER_ACCOUNT, <<"That Twitter account does not exist.">>).
-define(MSG_NO_TWITCOIN_ACCOUNT, <<"That Twitcoin account does not exist.">>).

-define(IBROWSE_OPTS, [{response_format, binary}]).

-type account_data() :: [{account_data_key(), account_data_value()}].
-type account_data_key() :: address | balance | screen_name | name
                            | profile_img_url.
-type account_data_value() :: binary() | float().
