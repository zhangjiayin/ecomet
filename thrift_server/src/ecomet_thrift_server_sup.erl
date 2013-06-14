
-module(ecomet_thrift_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok,Leader} = application:get_env(leader),
    net_adm:ping(Leader),
    error_logger:info_msg("~w===================|||||||||||||||", [Leader]),
    io:format("=========================sup\n"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    io:format("=========================sup init\n"),
    Children = [ ?CHILD(ecomet_thrift_server_controller, worker) ],
    {ok, { {one_for_one, 5, 10}, Children} }.
