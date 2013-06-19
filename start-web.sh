#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa apps/web/ebin apps/shared_module/ebin/ edit deps/*/ebin -boot start_sasl \
    -name "n1@127.0.0.1" \
    -setcookie abc \
    -config  ecomet.config \
    -K true \
    --hidden \
    -P 134217727 \
    -s ecomet \
    -s lager \
    -s reloader
