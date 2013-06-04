#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname "erouter2@localhost" \
    -setcookie abc \
    -mnesia dir '"/Users/zhangjiayin/dev/OpensourceSoftware/mochiweb/mnesia2"' \
    -K true \
    -P 134217727 \
    -s mnesia start   \
    -s ecomet_router_app 
    -s reloader
#    -config erouter2.config  \
   #-sname ecomet_dev \
   #-s ecomet \
   #-s reloader
