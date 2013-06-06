#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -name "erouter@10.32.232.28" \
    -setcookie abc \
    -mnesia dir '"./mnesia"' \
    -K true \
    -P 134217727 \
    -s mnesia start  \
    -s ecomet_router_app
   # -s reloader
 #   -config erouter.config \
   # -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
