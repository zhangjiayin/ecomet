
-module(ecomet_thrift_server_controller).
-include("../../shared_module/src/ecometRouter_thrift.hrl").
-include("../../shared_module/src/ecomet_router_types.hrl").

-export([start_link/0, stop/1,start/0,
         handle_function/2,
         send/1,send/4, send/3,
         get_online_count/1, get_online_ids/1,
         handle_error/2
% Thrift implementations
% FILL IN HERE
         ]).

%%%%% EXTERNAL INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start() ->
    start_link().

start_link() ->
    thrift_server:start_link(get_port(), ecometRouter_thrift, ?MODULE).

stop(Server) ->
    thrift_server:stop(Server),
    ok.

%%%%% THRIFT INTERFACE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(Function, Args) when is_atom(Function), is_tuple(Args) ->
%%    error_logger:info_msg("Function : ~w, arag ~w ", [Function,tuple_to_list(Args)]),
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok -> ok;
        Reply -> {reply, Reply}
    end.

%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send (Message) ->
    {Appid, To, Offline,Expire } = {Message#message.appId, Message#message.to, Message#message.offline,Message#message.expire},
    io:format("==============~p,~p,~p,~p,~p\n", [Appid,To,Offline,Expire,Message]),
    gen_server:call(pg2:get_closest_pid(erouter), {send,Appid,To,Message}).

send(Appid, Id, Msg)->
    gen_server:call(pg2:get_closest_pid(erouter), {send,Appid,Id,binary_to_list(Msg)}),
    
    ok.
send(Appid, Id,Msg,Offline)->
    gen_server:call(pg2:get_closest_pid(erouter), {send,Appid,Id,binary_to_list(Msg),Offline}),
    ok.

get_online_count(Appid) ->
    gen_server:call(pg2:get_closest_pid(erouter), {get_online_count,Appid}).

get_online_ids(Appid) ->
    gen_server:call(pg2:get_closest_pid(erouter), {get_online_ids,Appid}).

get_port() ->
    9999.
    %%{ok, Result} = application:get_env(ecomet_thrift_server, service_port),
handle_error(Function , Reason) ->
    error_logger:info_msg("Function: ~w, Reason~w ~n", [Function,Reason]),
    ok.
