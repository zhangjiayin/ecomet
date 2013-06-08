#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -name "erouter_server@127.0.0.1" \
    -setcookie abc \
    -K true \
    -P 134217727 \
    -s ecomet_thrift_server
   # -s reloader
 #   -config erouter.config \
   # -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
