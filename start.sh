#!/bin/sh
cd `dirname $0`

export SASL_CONFIG="$(erl -noshell -pa ebin -eval "error_logger:tty(false), twitcoin_app:set_config(), io:format(\"~s~n\",[twitcoin:get_app_env(twitcoin_sasl_config)])" -run init stop)"
export HEART_COMMAND="$(erl -noshell -pa ebin -eval "error_logger:tty(false), twitcoin_app:set_config(), io:format(\"~s~n\",[twitcoin:get_app_env(twitcoin_heart_command)])" -run init stop)"

exec erl -heart -detached -pa $PWD/ebin $PWD/deps/*/ebin \
    -config ${SASL_CONFIG} -s twitcoin
