%% @author Mochi Media <dev@mochimedia.com>
%% @copyright ecomet Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the ecomet application.

-module(ecomet_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ecomet.
start(_Type, _StartArgs) ->
    ecomet_deps:ensure(),
    ecomet_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ecomet.
stop(_State) ->
    ok.
