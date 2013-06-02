-module(ecomet_router_app).

-behaviour(application).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-export([start/0, start/2, stop/1]).

start() -> application:start(ecomet_router).

start(_StartType, _StartArgs) ->
    Servers = ['erouter@localhost', 'erouter1@localhost'],
    lists:foreach(fun(X)->net_adm:ping(X) end, Servers),
    error_logger:info_msg("Starting ecomet_router application...~n"),
    ecomet_router_sup:start_link().

stop(_State) ->
    ok.
