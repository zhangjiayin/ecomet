#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname "erouter@localhost" \
    -setcookie abc \
    -mnesia dir '"/Users/zhangjiayin/dev/OpensourceSoftware/mochiweb/mnesia"' \
    -K true \
    -P 134217727 \
    -s mnesia start  \
    -s ecomet_router_app \
   # -s reloader
 #   -config erouter.config \
   # -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
