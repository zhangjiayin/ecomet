#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname "erouter_server@localhost" \
    -setcookie abc \
    -K true \
    -P 134217727 \
    -s ecomet_thrift_server
   # -s reloader
 #   -config erouter.config \
   # -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
