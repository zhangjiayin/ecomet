-module(ecomet_offline).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         first_run/0,
     terminate/2, code_change/3]).

-export([store/3,get_msg/2, delete/2]).

-record(state, {}). % state is all in mnesia
-record(offline_msg, {id,appid,msg}).

-define(SERVER, ?MODULE).
-define(TABLE_OFFLINE, offline_msg).

start_link() ->
    case gen_server:start_link({local,?SERVER},?MODULE, [], []) of
        {ok, Pid} -> 
            {ok, Pid};
        {error, {already_started, Pid}} ->  
            link(Pid), 
            {ok, Pid};
        Else -> Else
    end.

% sends Msg to anyone logged in as Id
store(Appid, Id, Msg) ->
    gen_server:call(?SERVER, {store, Appid, Id, Msg}).

get_msg(Appid, Id) ->
    gen_server:call(?SERVER, {get_msg, Appid, Id}).

delete(Appid, Id) ->
    gen_server:call(?SERVER, {delete,Appid, Id}).
init([]) ->
    process_flag(trap_exit, true),
    ok = mnesia:start(),
    error_logger:info_msg("Waiting on mnesia tables..\n",[]),
    mnesia:wait_for_tables([offline_msg], 30000),
    Info = mnesia:table_info(?TABLE_OFFLINE, all),
    error_logger:info_msg("OK. Subscription table info: \n~w\n\n",[Info]),
    {ok, #state{} }.

handle_call({store, Appid, Id, Msg}, _From, State)  ->
    mnesia:dirty_write(#offline_msg{id=Id,appid=Appid, msg=Msg}),
    {reply, ok, State};
handle_call({get_msg, Appid, Id}, From, State) ->
    Msgs = mnesia:dirty_match_object(#offline_msg{id=Id,appid=Appid, msg='_'}),
    case Msgs of
        [] -> 
            [];
        _ ->
            Msgret = [Msg || #offline_msg{id=_,appid=_, msg=Msg} <- Msgs],
            gen_server:reply(From, Msgret),
            ok
    end,
    {reply, ok, State};
handle_call({delete, Appid,Id}, _From, State) ->
    Delete=#offline_msg{id=Id,appid=Appid, _='_'},
    Fun = fun() ->
            List = mnesia:match_object(Delete),
            lists:foreach(fun(X) ->
                        mnesia:delete_object(X)
                end, List)
    end,
    {reply, mnesia:transaction(Fun), State}.
% handle death and cleanup of logged in processes
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

first_run() ->
    Ret = mnesia:create_table(?TABLE_OFFLINE,
        [
       %% {disc_copies, [node()|nodes()]},
       %%{ramp_copies, [node()|nodes()]},
      %% {ramp_copies, [node()]},
     {attributes, record_info(fields, offline_msg)},
     {index, [appid]}, %index subscribee too
     {type, bag}
    ]),
    Ret.
