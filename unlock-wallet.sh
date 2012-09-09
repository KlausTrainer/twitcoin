#!/bin/sh

# As there seems to be no simple way to turn off stdin echo from within an
# escript, we use this shell script as a wrapper in order to do that.
# c.f. http://erlang.org/pipermail/erlang-questions/2011-November/062778.html

cd `dirname $0`
stty -echo
escript ./src/unlock_wallet.escript
stty echo
