%%%-------------------------------------------------------------------
%% @doc fuge server
%% @end
%%%-------------------------------------------------------------------

-module(fuge_server).

-behaviour(gen_server).

-include("fuge.hrl").

-export([start_link/0,
         new/3,
         get/1,
         experiment/2
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-type startlink_ret() :: {ok, pid()} | ignore | {error, term()}.

%% ETS table used for storing all experiments
-define(STORE, fuge_server_store).

%%-------------------------------------------------------------------
%% API
%%-------------------------------------------------------------------

-spec start_link() -> startlink_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec new(fuge:name(), list(), list()) -> ok | fuge:error().
new(Name, Subscribers, Options) ->
    gen_server:call(?MODULE, {new, Name, Subscribers, Options}).

-spec get(fuge:name()) -> {ok, fuge:fuge()} | fuge:error().
get(Name) ->
    case ets:lookup(?STORE, Name) of
        [] ->
            {error, not_found};
        [{Name, Fuge}] ->
            {ok, Fuge}
    end.

-spec experiment(fuge:fuge(), fuge:result()) -> ok.
experiment(Fuge, Result) ->
    gen_server:cast(?MODULE, {result, Fuge, Result}).

%%-------------------------------------------------------------------
%% gen_server callback implementation
%%-------------------------------------------------------------------

init([]) ->
    ets:new(?STORE, [named_table, {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({new, Name, Subscribers, Options}, _From, State) ->
    case ?MODULE:get(Name) of
        {error, not_found} ->
            Fuge = create_fuge(Name, Subscribers, Options),
            true = ets:insert_new(?STORE, {Name, Fuge}),
            lists:foreach(
              fun (Subscriber) ->
                      true = ets:insert_new(?STORE,
                                            {subscriber_key(Name, Subscriber),
                                             subscriber_value(Fuge, Subscriber)})
              end, Subscribers),
            {reply, ok, State};
        {ok, _Fuge} ->
            {reply, {error, already_exists}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({result, Fuge, Result}, State) ->
    lists:foreach(fun (SubscriberName) ->
                          handle_subscriber(Fuge, SubscriberName, Result)
                  end, Fuge#fuge.subscribers),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

create_fuge(Name, Subscribers, Options) ->
    Opts = handle_options(Options),
    #fuge{name = Name,
          subscribers = Subscribers,
          options = Opts}.

% TODO whitelist options, do stuff for specific options
handle_options(Options) ->
    Options.

subscriber_key(Name, {SubscriberName, _SubscriberState}) ->
    {Name, SubscriberName};
subscriber_key(Name, SubscriberName) ->
    {Name, SubscriberName}.

subscriber_value(Fuge, {SubscriberName, SubscriberState}) ->
    NewState = SubscriberName:init(Fuge, SubscriberState),
    #fuge_subscriber{name = SubscriberName,
                     state = NewState};
subscriber_value(Fuge, SubscriberName) ->
    subscriber_value(Fuge, {SubscriberName, undefined}).

get_subscriber(Name, SubscriberName) ->
    case ets:lookup(?STORE, subscriber_key(Name, SubscriberName)) of
        [] ->
            {error, not_found};
        [{_Key, Subscriber}] ->
            Subscriber
    end.

handle_subscriber(Fuge, SubscriberName, Result) ->
    % TODO try catch?
    Subscriber = get_subscriber(Fuge#fuge.name, SubscriberName),
    SubscriberName:handle_result(Fuge,
                                 Subscriber#fuge_subscriber.state,
                                 Result).
