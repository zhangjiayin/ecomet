#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa apps/router/ebin/ edit deps/*/ebin -boot start_sasl \
    -name "ecomet_router@127.0.0.1" \
    -setcookie abc \
    -mnesia dir '"./mnesia"' \
    -config ecomet.config \
    -K true \
    -P 134217727 \
    -s mnesia start  \
    -s lager  \
    -s ecomet_router_app
   # -s reloader
 #   -config erouter.config \
   # -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
