ecomet
======

An comet implemention   support by mochiweb  and mnesia in erlang  

with an pub/sub model  and  offline message implemention

TODO

1. fix types Appid  :integer()  Uid: integer()  Msg: json()
2. distribution router
3. performance test
4. fix lager config
5. change some Function support cast call 

webtools start

webtool:start(".//lib/webtool-0.8.9.2/priv/", [{port, 8889}, {bind_address,
         {0,0,0,0}}]).


getting start 


1. clone code 
         
         git clone https://github.com/zhangjiayin/ecomet.git

2. make and start router

         cd ecomet/router
         make
         ./rebar generate
         cd rel/ecomet_router/
         ./bin/ecomet_router start

3. make and start web

         cd ../../../web
         make
         ./rebar generate
         cd rel/ecomet_web/
         ./bin/ecomet_web start
         
4. make and start thrift server
         
         cd ../../../thrift_server/
         make
         ./rebar generate  
         cd rel/ecomet_thrift_server/
         ./bin/ecomet_thrift_server start

5. edit nginx conf include nginx config (demo.conf), and setup demo env

         cd ../../../demo/conf/
         #edit demo.conf change the root and fix fastcgi params
         #ensure  the php have pdo and sqlite driver
         #and then edit hosts file to  use hostname  demo to your test ip

6. enjoy it
