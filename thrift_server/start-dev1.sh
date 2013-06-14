#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -name "ecomet_thrift_server@127.0.0.1" \
    -setcookie abc \
    -config ecomet.config \
    -leader 'ecomet_router@127.0.0.1' \
    -K true \
    -P 134217727 \
    -s ecomet_thrift_server