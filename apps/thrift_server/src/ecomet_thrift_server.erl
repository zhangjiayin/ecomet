%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc ecomet.

-module(ecomet_thrift_server).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the ecomet server.
start() ->
    %%ecomet_deps:ensure(),
    ensure_started(crypto),
    ensure_started(lager),
    application:start(ecomet_thrift_server).


%% @spec stop() -> ok
%% @doc Stop the ecomet server.
stop() ->
    application:stop(ecomet_thrift_server).
