-module(ecomet_subsmanager).
-behaviour(gen_server).
-include_lib("stdlib/include/qlc.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_subscriptions/2,
         remove_subscriptions/2,
         get_subscribers/2,
         first_run/0,
         stop/0,
         start_link/0]).
-record(subscription, {subscriber, appid, subscribee}).
-record(state, {}). % state is all in mnesia
-define(SERVER, ?MODULE).

start_link() ->
    case gen_server:start_link({local, ?SERVER},?MODULE, [], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            link(Pid),
            {ok, Pid};
        Else -> Else
    end.


stop() ->
    gen_server:call(?MODULE, {stop}).

add_subscriptions(Appid, SubsList) ->
    gen_server:call(?MODULE, {add_subscriptions,Appid, SubsList}, infinity).

remove_subscriptions(Appid,SubsList) ->
    gen_server:call(?MODULE, {remove_subscriptions,Appid, SubsList}, infinity).

get_subscribers(Appid,User) ->
    gen_server:call(?MODULE, {get_subscribers, Appid, User}).


init([]) ->
    ok = mnesia:start(),
    error_logger:info_msg("Waiting on mnesia tables..\n",[]),
    mnesia:wait_for_tables([subscription], 30000),
    Info = mnesia:table_info(subscription, all),
    error_logger:info_msg("OK. Subscription table info: \n~w\n\n",[Info]),
    {ok, #state{} }.

handle_call({stop}, _From, State) ->
    {stop, stop, State};

handle_call({add_subscriptions, Appid, SubsList}, _From, State) ->
    % Transactionally is slower:
    % F = fun() ->
    %         [ ok = mnesia:write(S) || S <- SubsList ]
    %     end,
    % mnesia:transaction(F),
    [ mnesia:dirty_write(#subscription{subscriber=Ber,appid=Appid, subscribee=Bee}) || {Ber, Bee} <- SubsList ],
    {reply, ok, State};

handle_call({remove_subscriptions, Appid, SubsList}, _From, State) ->
    F = fun() ->
            [ ok = mnesia:delete_object(#subscription{subscriber=Ber,appid=Appid, subscribee=Bee}) || {Ber,Bee} <- SubsList ]
    end,
    mnesia:transaction(F),
    {reply, ok, State};

handle_call({get_subscribers,Appid, User}, From, State) ->
    F = fun() ->
        Subs = mnesia:dirty_match_object(#subscription{subscriber='_', appid=Appid, subscribee=User}),
        error_logger:info_msg("~w",[Subs]),
        Users = [Dude || #subscription{subscriber=Dude, appid=_, subscribee=_} <- Subs],
        gen_server:reply(From, Users)
    end,
    spawn(F),
    {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    mnesia:stop(),
    ok.

code_change(_OldVersion, State, _Extra) ->
    error_logger:info_msg("Reloading code for ?MODULE\n",[]),
    {ok, State}.


first_run() ->
    Ret = mnesia:create_table(subscription,
    [
     {ram_copies, [node()|nodes()]},
     {attributes, record_info(fields, subscription)},
     {index, [appid, subscribee]}, %index subscribee too
     {type, bag}
    ]),
    Ret.


