%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ecomet.

-module(ecomet_web).
-author("zhangjiayin <zhangjiayin99@gmail.com>").

-export([start/0, stop/0, feed/3, loop/3]).

-export([resume/4, msg_send/1]).
-define(WAITTIME, 30000).
%% External API

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
    io:format("keepalive ~w\n", [Keepalive]),
    process_flag(trap_exit, true),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "get_online_ids/" ++ _Id ->
                        Ids = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_ids,[1]),
                        IdCount = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_count,[1]),
                        error_logger:info_msg("~w", [Ids]),
                        error_logger:info_msg("~w", [IdCount]),
                        Json=mochijson2:encode({struct, [{ids,  [ list_to_binary(Iid)  ||Iid <-Ids]}, {count,IdCount }]}),

                        Req:ok({_ContentType = "application/json",
                                _Headers = [],
                                Json});
                    "ecomet/" ++ Id ->
                        Response = Req:ok({"text/html; charset=utf-8",
                                [{"Server","Mochiweb-Test"}],
                                chunked}),
                        erlang:send_after(?WAITTIME, self(), "ping"),
                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1, Id,self(),true]),
                        proc_lib:hibernate(?MODULE, feed, [Response, Id, 1]);
                    "longpoll/" ++ Id      ->
                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1,Id,self(),true]),
                        TimerRef = erlang:start_timer(?WAITTIME,self(), "ping"),
                        error_logger:error_report(["loop/2"]),
                        Reentry = mochiweb_http:reentry({?MODULE, loop,[DocRoot,keepalive]}),

                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef]),
                        io:format("not gonna happen~n", []);
                    _ ->
                        io:format("DocRoot ~p\n", [DocRoot]),
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    "send/" ++ _T -> 
                        msg_send(Req),
                        ok(Req,"ok");
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
            error_logger:error_report(Report),
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API
feed(Response,Id,N) ->
    receive
    {router_msg, Msg} ->
        Html = io_lib:format("Recvd msg #~w: '~s'", [N, Msg]),
        Response:write_chunk(Html);
    Msg1 ->
        io:format("Msg1 ~w",[Msg1]),
        ok
    after 1000 ->
            ok
    end,
    proc_lib:hibernate(?MODULE, feed, [Response, Id, 1]).

resume(Req, Id, Reentry,TimerRef) ->
    error_logger:error_report(["resume/4"]),
    receive
        {router_msg, Msg} ->
            erlang:cancel_timer(TimerRef),
            ok(Req, Msg);
        {'EXIT',_Pid,noconnection} ->
            rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[1,1,Id,self(),true]),
            ok;
        {timeout, _Pid, Msg} ->
            Json=mochijson2:encode({struct, [{type,ping},{msg,list_to_binary(Msg)}]}),
            ok(Req, Json);
        Msg ->
            erlang:cancel_timer(TimerRef),
            Text = iolib:format("~w", [Msg]),
            Json=mochijson2:encode({struct, [{type,ping},{msg,Text}]}),
            ok(Req, Json)
    end,

    io:format("reentering loop via continuation in ~p~n", [Req:get(path)]),
    Reentry(Req).

msg_send(Req) ->
    Args = Req:parse_post(),
    F = proplists:get_value("from", Args, "ana"),
    T = proplists:get_value("to", Args, "to"),
    M = proplists:get_value("msg", Args , "nothing"),

    Json=mochijson2:encode({struct, [{type,msg},{from,list_to_binary(F)}, {to, list_to_binary(T)},{msg,list_to_binary(M)}]}),
    rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, send,[1,T,Json]).

ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).
