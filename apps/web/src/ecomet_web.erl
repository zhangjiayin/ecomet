%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ecomet.

-module(ecomet_web).
-author("zhangjiayin <zhangjiayin99@gmail.com>").

-export([start/0, stop/0, feed/3, loop/3]).

-export([resume/4 ]).
-define(WAITTIME, 30000).
-include("../../shared_module/src/ecomet_router_types.hrl").
%% External API

%%-record(message, {appId :: integer(),
%%                 from :: integer(),
%%                 to :: integer(),
%%                 nick = "" :: string() | binary(),
%%                 type = "msg" :: string() | binary(),
%%                 content :: string() | binary(),
%%                 created = 0 :: integer(),
%%                 offline = false :: boolean(),
%%                 expire = 0 :: integer()}).

start() ->
    {ok,DocRoot} = application:get_env(docroot),
    {ok,ClusterNodes} = application:get_env(connect_nodes),
    {ok,Options} = application:get_env(server_config),
    lists:foreach(fun(X)->net_adm:ping(X) end, ClusterNodes),
    pg2:start_link(),
    process_flag(trap_exit,true),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot,nonekeepalive)
           end,
        mochiweb_http:start([{max,1000000},{name, ?MODULE}, {loop, Loop} | Options]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot,Keepalive) ->
    error_logger:info_msg("keepalive ~w\n", [Keepalive]),
    process_flag(trap_exit, true),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "get_online_ids/" ++ _Id ->
                        Ids = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_ids,[1]),
                        IdCount = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_count,[1]),
                        Json=mochijson2:encode({struct, [{ids,  [ Iid  ||Iid <-Ids]}, {count,IdCount }]}),
                        okJson(Req,Json),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive]}),
                        Reentry(Req);
                    "ecomet/" ++ Id ->
                        Response = Req:ok({"text/html; charset=utf-8",
                                [{"Server","ECOMET"}],
                                chunked}),
                        erlang:send_after(?WAITTIME, self(), "ping"),
                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1, Id,self(),true]),
                        proc_lib:hibernate(?MODULE, feed, [Response, Id, 1]);
                    "longpoll/" ++ Id     ->

                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1,list_to_integer(Id),self(),true]),
                        TimerRef = erlang:start_timer(?WAITTIME,self(), "ping"),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive]}),

                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef]);
                    "heathy/"           ->
                        Json=mochijson2:encode(heathy()),
                        okJson(Req, Json);
                    _ ->
                        error_logger:info_msg("DocRoot ~p\n", [DocRoot]),
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "longpoll/" ++ Appid   ->
                        Args = Req:parse_post(),
                        Id  = proplists:get_value("uid", Args, ""),
                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[list_to_integer(Appid),1,list_to_integer(Id),self(),true]),
                        TimerRef = erlang:start_timer(?WAITTIME,self(), "ping"),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive]}),

                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef]);
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:info_msg("error", [Report]),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API
feed(Response,Id,_N) ->
    receive
    {router_msg, Msg} ->
        J={struct, [{appid, Msg#message.appId},{nick, Msg#message.nick},{type,Msg#message.type},{content,Msg#message.content},{from, Msg#message.from},{to, Msg#message.to}, {created,Msg#message.created}]},
        Response:write_chunk(mochijson2:encode(J));
    Msg1 ->
        error_logger:info_msg("Msg1 ~w",[Msg1]),
        ok
    after 1000 ->
            ok
    end,
    proc_lib:hibernate(?MODULE, feed, [Response, Id, 1]).

resume(Req, Id, Reentry,TimerRef) ->
    error_logger:info_msg("resume/4"),
    receive
        {router_msg, Msg} ->
            erlang:cancel_timer(TimerRef),
            error_logger:info_msg("router_msg Msg  ~p~n", [Msg]),
            J={struct, [{appid, Msg#message.appId},{nick, Msg#message.nick},{type,Msg#message.type},{content,Msg#message.content},{from, Msg#message.from},{to, Msg#message.to}, {created,Msg#message.created}]},
            okJson(Req, mochijson2:encode(J));
        {'EXIT',Pid,noconnection} ->
            error_logger:info_msg("EXIT ~p~n", [Pid]),
            rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1,Id,self(),true]),
            ok;
        {timeout, _Pid, Msg} ->
            error_logger:info_msg("Timeout msg ~p~n", [Msg]),
            erlang:cancel_timer(TimerRef),
            Json=mochijson2:encode({struct, [{type,ping},{msg,list_to_binary(Msg)}]}),
            okJson(Req, Json);
        Msg ->
            error_logger:info_msg("uncatch msg ~p~n", [Msg]),
            erlang:cancel_timer(TimerRef),
            Text = iolib:format("~w", [Msg]),
            Json=mochijson2:encode({struct, [{type,ping},{msg,Text}]}),
            ok(Req, Json)
    end,

    error_logger:info_msg("reentering loop via continuation in ~p~n", [Req:get(path)]),
    Reentry(Req).

okJson(Req,Response) ->
    Req:ok({_ContentType = "application/json",
            _Headers = [{"Server","ECOMET"}, {"Access-Control-Allow-Origin", "*"}],
            Response}).


ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).

heathy () ->
    [
        {process_count,erlang:system_info(process_count)},
        {cpu,erlang:system_info(cpu_topology)},
        {memory, erlang:memory()},
        {nodes, nodes()}
    ].
