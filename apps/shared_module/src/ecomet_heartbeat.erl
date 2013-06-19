-module(ecomet_heartbeat).

%% erlang heart module is usefull

-export([start_link/0, loop/0]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, spawn_link(?MODULE, loop, [])}.

loop() ->
    receive  
        Pattern  ->  
            error_logger:info_msg(Pattern)
    after 5000 ->
            N = pg2:get_closest_pid(erouter),
            case N of 
                {error,_} ->
                    {ok,ClusterNodes} = application:get_env(connect_nodes),
                    error_logger:info_msg("cluster Nodes is  ~p ",[ClusterNodes]),
                    lists:foreach(fun(X)-> net_adm:ping(X) end, ClusterNodes);
                Pid ->
                    error_logger:info_msg("erouter is  ~p ",[Pid]),
                    ok
            end
    end ,
    loop().

