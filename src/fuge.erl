%%%-------------------------------------------------------------------
%% @doc fuge public interface
%% @end
%%%-------------------------------------------------------------------

-module(fuge).

-include("fuge.hrl").

-export([new/1, new/2, new/3,
         run/3, run/5, run/4]).

-export_type([fuge/0,
              name/0,
              error/0,
              result/0,
              subscriber_state/0
             ]).

-type fuge() :: #fuge{}.
-type result() :: #fuge_result{}.
-type subscriber_state() :: any().
-type name() :: term().
-type error() :: {error, atom()}.

-define(DEFAULT_SUBSCRIBERS, [fuge_subscriber_logger]).

-define(DEFAULT_OPTIONS, []).

-spec new(name()) -> ok | error().
new(Name) ->
    new(Name, ?DEFAULT_SUBSCRIBERS).

-spec new(name(), list()) -> ok | error().
new(Name, Subscribers) ->
    new(Name, Subscribers, ?DEFAULT_OPTIONS).

-spec new(name(), list(), list()) -> ok | error().
new(Name, Subscribers, Options) ->
    fuge_server:new(Name, Subscribers, Options).

-spec run(name(), fun(), fun()) -> any().
run(Name, Control, Candidate) ->
    run(Name, Control, Candidate, undefined, []).

-spec run(name(), fun(), fun(), any()) -> any().
run(Name, Control, Candidate, Context) ->
    run(Name, Control, Candidate, Context, []).

-spec run(name(), fun(), fun(), any(), list()) -> any().
run(Name, Control, Candidate, Context, Options) ->
    case fuge_server:get(Name) of
        {ok, Fuge} ->
            do_run(Fuge, Control, Candidate, Context, Options);
        {error, not_found} ->
            Control()
    end.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

do_run(Fuge, Control, Candidate, Context, Options) ->
    % TODO random runs
    ControlData = do_run_experiment(Control, Options),
    CandidateData = do_run_experiment(Candidate, Options),
    Result = #fuge_result{context = Context,
                          control = ControlData,
                          candidate = CandidateData,
                          execution_order = [0, 1]},
    fuge_server:experiment(Fuge, Result),
    ControlData#fuge_data.value.

% TODO think and use sane options
do_run_experiment(Fun, _Options) ->
    Start = os:timestamp(),
    % TODO try catch
    Result = Fun(),
    End = os:timestamp(),
    #fuge_data{duration = timer:now_diff(End, Start),
               exception = undefined,
               value = Result}.
