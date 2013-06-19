ecomet
======

An comet implemention   support by mochiweb  and mnesia in erlang  

with an pub/sub model  and  offline message implemention

TODO

1. distribution router
2. performance test
3. fix lager config
4. change some Function support cast call 

webtools start

webtool:start(".//lib/webtool-0.8.9.2/priv/", [{port, 8889}, {bind_address,
         {0,0,0,0}}]).


getting start 


1. clone code 
         
         git clone https://github.com/zhangjiayin/ecomet.git

2. make  and compile

        make 
        ./rebar generate 

3. start router

        cd target
        cd ecomet_router
        ./bin/ecomet_router start
        cd ../../

4. start web

        cd target
        cd ecomet
        ./bin/ecomet start
        cd ../../

5. start thrift server

        cd target
        cd ecomet_thrift_server
        ./bin/ecomet_thrift_server start
        cd ../../

6. edit nginx conf include nginx config (demo.conf), and setup demo env

         cd demo/conf/
         #edit demo.conf change the root and fix fastcgi params
         #ensure  the php have pdo and sqlite driver
         #and then edit hosts file to  use hostname  demo to your test ip
        cd ../../

7. enjoy it
