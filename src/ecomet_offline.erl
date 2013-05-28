-module(ecomet_offline).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         first_run/0,
     terminate/2, code_change/3]).

-export([store/2,get_msg/1, delete/1]).

-record(state, {}). % state is all in mnesia
-record(offline_msg, {id,msg}).

-define(SERVER, global:whereis_name(?MODULE)).
-define(TABLE_OFFLINE, offline_msg).

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
store(Id, Msg) ->
    gen_server:call(?SERVER, {store, Id, Msg}).

get_msg(Id) ->
    gen_server:call(?SERVER, {get_msg, Id}).

delete(Id) ->
    gen_server:call(?SERVER, {delete,Id}).
init([]) ->
    process_flag(trap_exit, true),
    ok = mnesia:start(),
    io:format("Waiting on mnesia tables..\n",[]),
    mnesia:wait_for_tables([offline_msg], 30000),
    Info = mnesia:table_info(?TABLE_OFFLINE, all),
    io:format("OK. Subscription table info: \n~w\n\n",[Info]),
    {ok, #state{} }.

handle_call({store, Id, Msg}, _From, State)  ->
    mnesia:dirty_write(#offline_msg{id=Id, msg=Msg}),
    {reply, ok, State};
handle_call({get_msg, Id}, From, State) ->
    Msgs = mnesia:dirty_match_object(#offline_msg{id=Id,msg='_'}),
    case Msgs of
        [] -> 
            ok;
        _ ->
            Msgret = [Msg || #offline_msg{id=_,msg=Msg} <- Msgs],
            gen_server:reply(From, Msgret),
            ok
    end,
    {reply, ok, State};
handle_call({delete, Id}, _From, State) ->
    mnesia:dirty_delete({?TABLE_OFFLINE,Id}),
    {reply, ok, State}.
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
    mnesia:create_schema([node()|nodes()]),
    ok = mnesia:start(),
    Ret = mnesia:create_table(?TABLE_OFFLINE,
    [{disc_copies, [node()|nodes()]},
     {attributes, record_info(fields, offline_msg)},
    %% {index, [id]}, %index subscribee too
     {type, bag}
    ]),
    Ret.
