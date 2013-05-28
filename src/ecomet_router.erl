-module(ecomet_router).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-export([send/2,send/3,publish/2,publish/3,login/2,login/3, logout/1]).

-define(SERVER, global:whereis_name(?MODULE)).

% will hold bidirectional mapping between id <--> pid
-record(state, {pid2id, id2pid}).

start_link() ->
    global:trans({?SERVER, ?SERVER},
        fun() ->
                case global:whereis_name(?MODULE) of
                    undefined ->
                        gen_server:start_link({global, ?MODULE}, ?MODULE, [], []);
                    _ ->
                        ok
                end
        end).

% sends Msg to anyone logged in as Id
send(Id, Msg) ->
    gen_server:call(?SERVER, {send, Id, Msg}).
send(Id, Msg,Offline) ->
    gen_server:call(?SERVER, {send, Id, Msg, Offline}).

publish(Id, Msg) ->
    gen_server:call(?SERVER, {publish, Id, Msg}).
publish(Id, Msg,Offline) ->
    gen_server:call(?SERVER, {publish, Id, Msg, Offline}).

login(Id, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {login, Id, Pid}).

login(Id, Pid, Offline) when is_pid(Pid) ->
    gen_server:call(?SERVER, {login, Id, Pid, Offline}).


logout(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {logout, Pid}).

%%

init([]) ->
    % set this so we can catch death of logged in pids:
    process_flag(trap_exit, true),
    % use ets for routing tables
    {ok, #state{
                pid2id = ets:new(?MODULE, [bag]),
                id2pid = ets:new(?MODULE, [bag])
               }
    }.

login_call(Id,Pid,State, Offline) when is_pid(Pid) -> 
    ets:insert(State#state.pid2id, {Pid, Id}),
    ets:insert(State#state.id2pid, {Id, Pid}),
    link(Pid), % tell us if they exit, so we can log them out
    io:format("~w logged in as ~w\n",[Pid, Id]),
    case Offline of 
        true ->
            Msgs = ecomet_offline:get_msg(Id),
            io:format("Msgs ~w\n",[Msgs]),
            case Msgs of
                [] ->
                    ok;
                ok ->
                    ok;
                _->
                    [ Pid ! {router_msg, Msg} || Msg <- Msgs ],
                    ecomet_offline:delete(Id),
                    ok
            end;
        _ ->
            ok
    end.

send_call (Id,Msg, State, Offline) ->
    % get pids who are logged in as this Id
    Pids = [ P || { _Id, P } <- ets:lookup(State#state.id2pid, Id) ],
    % send Msg to them all
    M = {router_msg, Msg},
    case Pids of
        [] ->
            case Offline of
                true ->
                    ecomet_offline:store(Id, Msg);
                _->
                    ok
            end,
            ok;
        _ ->
            [ Pid ! M || Pid <- Pids ]
    end.

publish_call (Id,Msg,Offline, From,State)->
    F = fun() ->
        % get users who are subscribed to Id:
        Users = ecomet_subsmanager:get_subscribers(Id),
        [ send_call(User,Msg, State, Offline) || User <- Users ],
        gen_server:reply(From, {ok, length(Users)})
    end,
    spawn(F),
    {noreply, State}.

handle_call({login, Id, Pid,Offline}, _From, State) when is_pid(Pid) ->
    login_call(Id,Pid,State, Offline),
    {reply, ok, State};
handle_call({login, Id, Pid}, _From, State) when is_pid(Pid) ->
    login_call(Id,Pid,State, false),
    {reply, ok, State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
    unlink(Pid),
    PidRows = ets:lookup(State#state.pid2id, Pid),
    case PidRows of
        [] ->
            ok;
        _ ->
            IdRows = [ {I,P} || {P,I} <- PidRows ], % invert tuples
            % delete all pid->id entries
            ets:delete(State#state.pid2id, Pid),
            % and all id->pid
            [ ets:delete_object(State#state.id2pid, Obj) || Obj <- IdRows ]
    end,
    io:format("pid ~w logged out\n",[Pid]),
    {reply, ok, State};

handle_call({publish, Id, Msg, Offline}, From, State) ->
    publish_call (Id, Msg, Offline, From,State),
    {noreply, State};

handle_call({publish, Id, Msg}, From, State) ->
    publish_call (Id, Msg, false, From,State),
    {noreply, State};

handle_call({send, Id, Msg,Offline}, _From, State) ->
    send_call(Id,Msg,State,Offline),
    {reply, ok, State};

handle_call({send, Id, Msg}, _From, State) ->
    Offline = false,
    send_call(Id,Msg,State,Offline),
    {reply, ok, State}.

% handle death and cleanup of logged in processes
handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, _Why} ->
            % force logout:
            handle_call({logout, Pid}, blah, State); 
        Wtf ->
            io:format("Caught unhandled message: ~w\n", [Wtf])
    end,
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
