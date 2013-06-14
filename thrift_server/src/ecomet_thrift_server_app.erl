-module(ecomet_thrift_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    A = application:get_all_env(),
    error_logger:info_msg("~w===================|||||||||||||||", [A]),
    ecomet_thrift_server_sup:start_link().

stop(_State) ->
    ok.
