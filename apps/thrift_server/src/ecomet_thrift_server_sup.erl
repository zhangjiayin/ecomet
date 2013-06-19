
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Children = [ ?CHILD(ecomet_thrift_server_controller, worker) , ?CHILD(ecomet_heartbeat, worker)],
    {ok, { {one_for_one, 5, 10}, Children} }.
