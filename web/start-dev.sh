#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    -sname "n1@localhost" \
    -setcookie abc \
    -mnesia dir '"/Users/zhangjiayin/dev/OpensourceSoftware/mochiweb/mnesia"' \
    -K true \
    --hidden \
    -P 134217727 \
    -s ecomet \
    -s reloader
   #-sname ecomet_dev \
   #-s ecomet \
   #-s reloader
