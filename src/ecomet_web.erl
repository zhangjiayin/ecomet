%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ecomet.

-module(ecomet_web).
-author("zhangjiayin <zhangjiayin99@gmail.com>").

-export([start/1, stop/0, loop/2, feed/3]).
%% External API

start(Options) ->
   case global:whereis_name(ecomet_router) of
       undefined ->
           ecomet_router:start_link()
   end,

   case global:whereis_name(ecomet_subsmanager) of
       undefined ->
           ecomet_subsmanager:start_link()
   end,

   case global:whereis_name(ecomet_offline) of
       undefined ->
           ecomet_offline:start_link()
   end,

    {DocRoot, Options1} = get_option(docroot, Options),
    {ClusterNodes, Options2} = get_option(cluster_nodes, Options1),
    lists:foreach(fun(X)->net_adm:ping(X) end, ClusterNodes),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
        mochiweb_http:start([{max,1000000},{name, ?MODULE}, {loop, Loop} | Options2]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "test/" ++ Id ->
                        Response = Req:ok({"text/html; charset=utf-8",
                                [{"Server","Mochiweb-Test"}],
                                chunked}),
                        % login using an integer rather than a string
                        {IdInt, _} = string:to_integer(Id),
                        ecomet_router:login(IdInt, self(),true),
                        proc_lib:hibernate(?MODULE, feed, [Response, IdInt, 1]);
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
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
    _ ->
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
