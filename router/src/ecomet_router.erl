-module(ecomet_router).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3, first_run/0, sync_db/1]).

-export([send/3,send/4,publish/3,publish/4,login/4,login/5, logout/1, get_online_count/1, get_online_ids/1]).

-define(SERVER, ?MODULE).

-define(TABLE_ONLINE, onlines).

-record(state, {}).

-record(onlines, {pid, appid, type, uid, ctime}).

start() ->
    ?MODULE:start_link().

start_link() ->
    mnesia:start(),
    {ok,Leader} = application:get_env(leader),
    net_adm:ping(Leader),

    case mnesia:create_table(?TABLE_ONLINE, []) of
         {atomic, ok} ->
             error_logger:info_msg("leader ~w, node ~w ok", [Leader, node()]),
             mnesia:delete_table(?TABLE_ONLINE),
             case (node() == Leader) of 
                 true ->
                     mnesia:stop(),
                     mnesia:delete_schema([node()]),
                     ?MODULE:first_run();
                 _ ->
                     error_logger:info_msg("==================I'm slave"),
                     ?MODULE:sync_db(Leader),
                     ok
             end;
         _  ->
             error_logger:info_msg("table exists ok")
     end,


    case gen_server:start_link({local, ?SERVER},?MODULE, [], []) of
        {ok, Pid} ->
             pg2:create(erouter),
             pg2:join(erouter, Pid),
            {ok, Pid};
        {error, {already_started, Pid}} ->
            link(Pid),
            {ok, Pid};
        Else -> Else
    end.

timestamp() ->
        calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

get_all_online_ids(Appid)->
   Us = mnesia:dirty_match_object(#onlines{pid='_',appid=Appid,type='_',uid='_',ctime='_'}),
   Users = [Uid || {onlines, _, _,_, Uid,_} <- Us],
   error_logger:info_msg("Users ~w ~w\n",[Us, Users]),
   Users.

get_online_count(Appid) ->
    gen_server:call(?MODULE, {get_online_count,Appid}).

get_online_ids(Appid) ->
    gen_server:call(?MODULE, {get_online_ids,Appid}).

send(Appid, Id, Msg) ->
    gen_server:call(?MODULE, {send, Appid, Id, Msg}).
send(Appid, Id, Msg,Offline) ->
    gen_server:call(?MODULE, {send, Appid, Id, Msg, Offline}).

publish(Appid, Id, Msg) ->
    gen_server:call(?MODULE, {publish, Appid, Id, Msg}).
publish(Appid, Id, Msg,Offline) ->
    gen_server:call(?MODULE, {publish, Appid, Id, Msg, Offline}).

login(Appid, Type, Id, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {login, Appid,Type,  Id, Pid}).

login(Appid,Type, Id, Pid, Offline) when is_pid(Pid) ->
    gen_server:call(?MODULE, {login, Appid, Type, Id, Pid, Offline}).


logout(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {logout, Pid}).

%%

init([]) ->
    process_flag(trap_exit, true),
    ok = mnesia:start(),
    error_logger:info_msg("Waiting on mnesia tables..\n",[]),
    mnesia:wait_for_tables([?TABLE_ONLINE], 30000),
    Info = mnesia:table_info(?TABLE_ONLINE, all),
    error_logger:info_msg("OK. Subscription table info: \n~w\n",[Info]),
    {ok, #state{} }.


login_call(Appid, Type, Uid, Pid,Offline) when is_pid(Pid) ->
    Onlines = #onlines{pid=Pid,appid=Appid,type=Type,uid=Uid,ctime= timestamp()},
    R =mnesia:dirty_write(Onlines),
    error_logger:info_msg("~w R= ~w",[Onlines, R]),
    link(Pid), % tell us if they exit, so we can log them out
    error_logger:info_msg("~w logged in as \n",[Onlines]),
    error_logger:info_msg("Offline ~w\n",[Offline]),
    case Offline of
        true ->
            Msgs = ecomet_offline:get_msg(Appid, Uid),
            error_logger:info_msg("Msgs ~w\n",[Msgs]),
            case Msgs of
                [] ->
                    ok;
                ok ->
                    ok;
                _->
                    error_logger:info_msg("Msgs ~w~n", Msgs),
                    [ Pid ! {router_msg, Msg} || Msg <- Msgs ],
                    ecomet_offline:delete(Appid, Uid),
                    ok
            end;
        _ ->
            ok
    end.

send_call (Appid, Uid,Msg,Offline) ->
    Pids = mnesia:dirty_match_object(#onlines{pid='_', appid=Appid,type='_', uid=Uid,ctime='_'}),
    % send Msg to them all
    M = {router_msg, Msg},
    error_logger:info_msg("pids~w~nAppid: ~w Uid, ~w ",[Pids,Appid,Uid]),
    case Pids of
        [] ->
            case Offline of
                true ->
                    ecomet_offline:store(Appid, Uid, Msg);
                _->
                    ok
            end,
            ok;
        _ ->
            error_logger:info_msg("~w",[Pids]),
            [Pid ! M || {onlines,Pid,_,_,_,_} <- Pids]
    end.

%%TODO
publish_call (Appid, Id,Msg,Offline, From,State)->
    F = fun() ->
            Users = ecomet_subsmanager:get_subscribers(Appid, Id),
            [ send_call(Appid, User, Msg, Offline) || User <- Users ],
            gen_server:reply(From, {ok, length(Users)})
    end,
    spawn(F),
    {noreply, State}.


handle_call({get_online_count,Appid}, _From, State) ->
    R=get_all_online_ids(Appid),
    {reply, length(R), State};

handle_call({get_online_ids,Appid}, _From, State) ->
    R=get_all_online_ids(Appid),
    {reply, R, State};

handle_call({login,Appid, Type,  Uid, Pid,Offline}, _From, State) when is_pid(Pid) ->
    {reply, login_call(Appid, Type,  Uid,Pid,Offline), State};
handle_call({login, Appid, Type,  Id, Pid}, _From, State) when is_pid(Pid) ->
    {reply,login_call(Id,Appid,Type, Pid,false), State};

handle_call({logout, Pid}, _From, State) when is_pid(Pid) ->
    unlink(Pid),
    PidRows = mnesia:dirty_match_object(#onlines{pid=Pid, appid='_',type='_',uid='_',ctime='_'}),
    case PidRows of
        [] ->
            A = ok,
            ok;
        _ ->
            A = mnesia:dirty_delete({?TABLE_ONLINE,Pid})
            %%[mnesia:dirty_delete_object(#online_ids{online_id=_Id, pid=Pid1}) || {_Id, Pid1}<- IdRows ]
    end,
    error_logger:info_msg("~w logout~n",PidRows),
    {reply, A, State};

handle_call({publish,Appid, Id, Msg, Offline}, From, State) ->
    publish_call (Appid, Id, Msg, Offline, From,State),
    {noreply, State};

handle_call({publish, Appid, Id, Msg}, From, State) ->
    publish_call (Appid, Id, Msg, false, From,State),
    {noreply, State};

handle_call({send, Appid, Id, Msg,Offline}, _From, State) ->
    send_call(Appid, Id,Msg,Offline),
    {reply, ok, State};

handle_call({send, Appid, Id, Msg}, _From, State) ->
    Offline = false,
    send_call(Appid, Id,Msg,Offline),
    {reply, ok, State}.

% handle death and cleanup of logged in processes
handle_info(Info, State) ->
    case Info of
        {'EXIT', Pid, Why} ->
            error_logger:info_msg("Why exit ~w~n", [ Why]),
            % force logout:
            handle_call({logout, Pid}, blah, State);
        Wtf ->
            error_logger:info_msg("Caught unhandled message: ~w\n", [Wtf])
    end,
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


first_run()->
   mnesia:create_schema([node()]),
   ok = mnesia:start(),
   R1= ecomet_offline:first_run(),
  error_logger:info_msg(R1),
  R2 = ecomet_subsmanager:first_run(),
  error_logger:info_msg(R2),

  R3=mnesia:create_table(?TABLE_ONLINE, [
          {ram_copies, [node()|nodes()]},
          {attributes, record_info(fields, onlines)},
          {index,[appid,uid]},
          {type, set}
      ]),

  error_logger:info_msg(R3),

  ok.

sync_db(N) ->
    mnesia:change_config(extra_db_nodes, [N]),

    mnesia:add_table_copy(schema, node(), disc_copies),
    mnesia:add_table_copy(offline_msg, node(), ram_copies),
    mnesia:add_table_copy(subscription, node(), ram_copies),
    mnesia:add_table_copy(?TABLE_ONLINE, node(), ram_copies),


    mnesia:change_table_copy_type(schema, node(), disc_copies),
    mnesia:change_table_copy_type(offline_msg, node(), ram_copies),
    mnesia:change_table_copy_type(subscription, node(), ram_copies),
    mnesia:change_table_copy_type(?TABLE_ONLINE, node(), ram_copies).

