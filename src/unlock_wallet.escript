#!/usr/bin/env escript

main(_) ->
    error_logger:tty(false),
    Dir = filename:dirname(filename:absname(escript:script_name())),
    code:add_path(Dir ++ "/../deps/ibrowse/ebin"),
    code:add_path(Dir ++ "/../deps/jiffy/ebin"),
    code:add_path(Dir ++ "/../ebin"),
    application:start(crypto),
    application:start(ibrowse),

    twitcoin_app:set_config(),
    BitcoindUrl = twitcoin:get_app_env(twitcoin_bitcoind_url),
    BasicAuth = {basic_auth,
        {
            twitcoin:get_app_env(twitcoin_bitcoind_rpc_user),
            twitcoin:get_app_env(twitcoin_bitcoind_rpc_password)
        }
    },
    twitcoin_bitcoind_client:start(BitcoindUrl, BasicAuth),

    Passphrase0 = io:get_line("enter passphrase: "),
    Passphrase = string:substr(Passphrase0, 1, length(Passphrase0) - 1),
    Result = twitcoin_bitcoind_client:walletpassphrase(Passphrase, 63072000), % two years
    case Result of
    {error, Error} ->
        io:format("~n~s~n", [binary_to_list(Error)]),
        halt(1);
    {ok, true} ->
        io:format("~nWallet unlocked.~n"),
        halt(0)
    end.
