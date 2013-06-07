%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ecomet.

-module(ecomet_web).
-author("zhangjiayin <zhangjiayin99@gmail.com>").

-export([start/1, stop/0, loop/2, feed/3, loop/1]).

-export([resume/4, msg_send/1]).
-define(LOOP, {?MODULE, loop}).
-define(WAITTIME, 30000).
%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    {ClusterNodes, Options2} = get_option(cluster_nodes, Options1),
    pg2:start_link(),
    lists:foreach(fun(X)->net_adm:ping(X) end, ClusterNodes),
    process_flag(trap_exit,true),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
        mochiweb_http:start([{max,1000000},{name, ?MODULE}, {loop, Loop} | Options2]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    process_flag(trap_exit, true),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "get_online_ids/" ++ _Id ->
                        Ids = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_ids,[]),
                        IdCount = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_count,[]),
                        Json=mochijson2:encode({struct, [{ids,  [ list_to_binary(Iid)  ||Iid <-Ids]}, {count,IdCount }]}),

                        Req:ok({_ContentType = "application/json",
                                _Headers = [],
                                Json});
                    "ecomet/" ++ Id ->
                        Response = Req:ok({"text/html; charset=utf-8",
                                [{"Server","Mochiweb-Test"}],
                                chunked}),
                        erlang:send_after(?WAITTIME, self(), "ping"),
                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[Id,self(),true]),
                        proc_lib:hibernate(?MODULE, feed, [Response, Id, 1]);
                    "longpoll/" ++ Id      ->
                        rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[Id,self(),true]),
                        TimerRef = erlang:start_timer(?WAITTIME,self(), "ping"),
                        error_logger:error_report(["loop/2"]),
                        Reentry = mochiweb_http:reentry(?LOOP),
                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef]),
                        io:format("not gonna happen~n", []);
                    _ ->
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
            %% NOTE: mustache templates need \ because they are not awesome.
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

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.

loop(Req) ->
    %%process_flag(trap_exit, true),
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "get_online_ids/" ++ _Id ->
                        Ids = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_ids,[]),
                        IdCount = rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, get_online_count,[]),
                        Json=mochijson2:encode({struct, [{ids,  [ list_to_binary(Iid)  ||Iid <-Ids]}, {count,IdCount }]}),

                        Req:ok({_ContentType = "application/json",
                                _Headers = [],
                                Json});
                    "longpoll/" ++ Id      ->
                        Reentry = mochiweb_http:reentry(?LOOP),
                        erlang:send_after(?WAITTIME, self(), "ping"),
                        error_logger:error_report(["loop/1"]),
                        error_logger:error_report([Id]),
                        %%proc_lib:hibernate(?MODULE, resume, [Req, Id, Reentry]);
                        TimerRef = erlang:start_timer(?WAITTIME,self(), "ping"),
                        proc_lib:hibernate(?MODULE, resume, [Req,Id, Reentry, TimerRef]);
                    _ ->
                        io:format("~w", [Req]),
                        Req:not_found()
                end;
            'POST' ->
                case Path of
                    "send/" -> 
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
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.


resume(Req, Id, Reentry,TimerRef) ->
    error_logger:error_report(["resume/4"]),
    receive
        {router_msg, Msg} ->
            erlang:cancel_timer(TimerRef),
            ok(Req, Msg);
        {'EXIT',_Pid,noconnection} ->
            rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, login,[Id,self(),true]),
            ok;
        {timeout, _Pid, Msg} ->
            Json=mochijson2:encode({struct, [{type,ping},{msg,list_to_binary(Msg)}]}),
            ok(Req, Json);
        Msg ->
            erlang:cancel_timer(TimerRef),
            %%Text = io_lib:format("message: ~p  Pid: ~p", [Msg, self()]),
            Json=mochijson2:encode({struct, [{type,ping},{msg,iolib:format("~w", [Msg])}]}),
            ok(Req, Json)
    %%after  ?WAITTIME ->
    %%    ok
    end,

    io:format("reentering loop via continuation in ~p~n", [Req:get(path)]),
    Reentry(Req).

msg_send(Req) ->
    Args = Req:parse_post(),
    F = proplists:get_value("from", Args, "ana"),
    T = proplists:get_value("to", Args, "to"),
    M = proplists:get_value("msg", Args , "nothing"),

    Json=mochijson2:encode({struct, [{type,msg},{from,list_to_binary(F)}, {to, list_to_binary(T)},{msg,list_to_binary(M)}]}),
    rpc:call(node(pg2:get_closest_pid(erouter)),ecomet_router, send,[T,Json]).

ok(Req, Response) ->
    Req:ok({_ContentType = "text/plain",
            _Headers = [],
            Response}).
