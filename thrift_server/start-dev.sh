#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin deps/*/ebin \
    -boot start_sasl \
    -name "ecomet_server1@127.0.0.1" \
    -setcookie abc \
    -config ecomet.config\
    -K true \
    -P 134217727 \
    -s ecomet_thrift_server 
