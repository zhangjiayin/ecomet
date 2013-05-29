#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname "erouter1@localhost" \
    -setcookie abc \
    -mnesia dir '"/Users/zhangjiayin/dev/OpensourceSoftware/mochiweb/mnesia_1"' \
    -K true \
    -P 134217727 \
    -s ecomet_router_app 
   # -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
   #-s reloader
