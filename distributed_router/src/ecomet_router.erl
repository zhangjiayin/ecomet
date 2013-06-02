-module(ecomet_router).
-behaviour(gen_server).

-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3, first_run/0]).

-export([send/2,send/3,publish/2,publish/3,login/2,login/3, logout/1]).

-define(SERVER, global:whereis_name(?MODULE)).

-define(TABLE_PIDS,online_pids).
-define(TABLE_IDS, online_ids).

-record(state, {}).

-record(online_pids, {online_pid, id}).

-record(online_ids, {online_id,pid}).

start() ->
    start_link().

start_link() ->
    global:trans({?MODULE, ?MODULE}, fun() ->
                case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
                    {ok, Pid} -> 
                        {ok, Pid};
                    {error, {already_started, Pid}} ->  
                        link(Pid), 
                        {ok, Pid};
                    Else -> Else
                end     
        end).


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

    ok = mnesia:start(),
    io:format("Waiting on mnesia tables..\n",[]),
    mnesia:wait_for_tables([?TABLE_PIDS, ?TABLE_IDS], 30000),
    Info = mnesia:table_info(?TABLE_PIDS, all),
    Info1 = mnesia:table_info(?TABLE_IDS, all),
    io:format("OK. Subscription table info: \n~w\n~w\n",[Info,Info1]),
    {ok, #state{} }.


login_call(Id,Pid,Offline) when is_pid(Pid) -> 
    Online_pids = #online_pids{online_pid=Pid,id=Id},
    Online_ids = #online_ids{online_id=Id, pid=Pid},
    mnesia:dirty_write(Online_ids),
    mnesia:dirty_write(Online_pids),
    link(Pid), % tell us if they exit, so we can log them out
    io:format("~w logged in as ~w\n",[Pid, Id]),
    io:format("Offline ~w\n",[Offline]),
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
                    io:format("Msgs ~w~n", Msgs),
                    [ Pid ! {router_msg, Msg} || Msg <- Msgs ],
                    ecomet_offline:delete(Id),
                    ok
            end;
        _ ->
            ok
    end.

send_call (Id,Msg,Offline) ->
    Pids = mnesia:dirty_match_object(#online_ids{online_id=Id,pid='_'}),
    % send Msg to them all
    M = {router_msg, Msg},
    io:format("pids~w~n",[Pids]),
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
            [Pid ! M || {online_ids,_,Pid} <- Pids] % invert tuples
    end.

publish_call (Id,Msg,Offline, From,State)->
    F = fun() ->
            Users = ecomet_subsmanager:get_subscribers(Id),
            [ send_call(User,Msg, Offline) || User <- Users ],
            gen_server:reply(From, {ok, length(Users)})
    end,
    spawn(F),
    {noreply, State}.

handle_call({login, Id, Pid,Offline}, _From, State) when is_pid(Pid) ->
    login_call(Id,Pid,Offline),
    {reply, ok, State};
handle_call({login, Id, Pid}, _From, State) when is_pid(Pid) ->
    login_call(Id,Pid,false),
    {reply, ok, State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
    unlink(Pid),
    PidRows = mnesia:dirty_match_object(#online_pids{online_pid=Pid, id='_'}),
    case PidRows of
        [] ->
            ok;
        _ ->
            io:format("pidrows~w~n",[PidRows]),
            IdRows = [ {I,P} || {online_pids,P,I} <- PidRows ], % invert tuples
            io:format("idrows~w~n",[IdRows]),
            mnesia:dirty_delete({?TABLE_PIDS,Pid}),
            [ mnesia:dirty_delete(?TABLE_IDS,Obj) || Obj <- IdRows ]
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
    send_call(Id,Msg,Offline),
    {reply, ok, State};

handle_call({send, Id, Msg}, _From, State) ->
    Offline = false,
    send_call(Id,Msg,Offline),
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

first_run()->
   mnesia:create_schema([node()|nodes()]),
   ok = mnesia:start(),
  R1= ecomet_offline:first_run(),
  error_logger:info_msg(R1),
  R2 = ecomet_subsmanager:first_run(),
  error_logger:info_msg(R2),

  R3=mnesia:create_table(?TABLE_PIDS, [
          {ram_copies, [node()|nodes()]}, 
          {attributes, record_info(fields, online_pids)},
          {type, bag}
      ]),

  error_logger:info_msg(R3),
  R4=mnesia:create_table(?TABLE_IDS,
      [
          {ram_copies, [node()|nodes()]},
          {attributes, record_info(fields, online_ids)},
          {type, bag}
      ]),
  error_logger:info_msg(R4),

 %%  lists:foreach(fun(X)->
 %%              mnesia:change_config(extra_db_nodes, [X]),
 %%              mnesia:change_table_copy_type(?TABLE_IDS, X, ramp_copies),
 %%              mnesia:change_table_copy_type(offline_msg, X, ramp_copies),
 %%              mnesia:change_table_copy_type(subscription, X, ramp_copies),
 %%              mnesia:change_table_copy_type(?TABLE_PIDS, X, ramp_copies) 
 %%      end, 
 %%      [node()|nodes()]),
    ok.
