#!/bin/sh
exec erl -pa ebin deps/*/ebin \
    -sname twitcoin_dev \
    -s twitcoin \
    -s reloader
