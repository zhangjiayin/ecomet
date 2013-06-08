
-module(ecomet_thrift_server).
-include("ecometRouter_thrift.hrl").
-include("ecomet_router_types.hrl").

-export([start_link/0, stop/1,start/0,
         handle_function/2,
         send/4, send/3,
         handle_error/2
% Thrift implementations
% FILL IN HERE
         ]).

%%%%% EXTERNAL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    net_adm:ping('erouter@127.0.0.1'),
    start_link().

start_link() ->
    thrift_server:start_link(get_port(), ecometRouter_thrift, ?MODULE).

stop(Server) ->
    thrift_server:stop(Server),
    ok.

%%%%% THRIFT INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(Appid, Id, Msg)->
    io:format("Appid,~w Id: ~w, Msg ~w ~n", [Appid, Id,Msg]),
    gen_server:call(pg2:get_closest_pid(erouter), {send,list_to_integer(binary_to_list(Appid)),binary_to_list(Id),binary_to_list(Msg)}),
    ok.
send(Appid, Id,Msg,Offline)->
    io:format("Id: ~w, Msg ~w Offline ~w~n", [Id,Msg,Offline]),
    gen_server:call(pg2:get_closest_pid(erouter), {send,Appid,Id,Msg,Offline}),
    ok.

get_port() ->
    9999.
    %%{ok, Result} = application:get_env(ecomet_thrift_server, service_port),
    %%Result.
handle_error(Function , Reason) ->
    io:format("Function: ~w, Reason~w ~n", [Function,Reason]),
    ok.
