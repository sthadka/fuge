%%%-------------------------------------------------------------------
%% @doc fuge public interface
%% @end
%%%-------------------------------------------------------------------

-module(fuge).

-include("fuge.hrl").

-export([new/1, new/2, new/3,
         run/3, run/4, run/5]).

-export_type([fuge/0,
              name/0,
              error/0,
              result/0,
              subscriber_state/0
             ]).

-define(DEFAULT_SUBSCRIBERS, [fuge_subscriber_logger]).
-define(DEFAULT_OPTIONS, []).

-spec new(name()) -> ok | error() | list(error()).
new(Name) ->
    new(Name, ?DEFAULT_SUBSCRIBERS).

-spec new(name(), list(subscriber())) -> ok | error() | list(error()).
new(Name, Subscribers) ->
    new(Name, Subscribers, ?DEFAULT_OPTIONS).

-spec new(name(), list(subscriber()), options()) -> ok | error() | list(error()).
new(Name, Subscribers, Options) ->
    case validate_options(Options) of
        [] ->
            fuge_server:new(Name, Subscribers, Options);
        Error ->
            Error
    end.

-spec run(name(), fun(), fun() | list(fun())) -> any().
run(Name, Control, Candidates) when is_list(Candidates) ->
    run(Name, Control, Candidates, undefined, []);
run(Name, Control, Candidate) ->
    run(Name, Control, [Candidate], undefined, []).

-spec run(name(), fun(), fun() | list(fun()), any()) -> any().
run(Name, Control, Candidates, Context) when is_list(Candidates) ->
    run(Name, Control, Candidates, Context, []);
run(Name, Control, Candidate, Context) ->
    run(Name, Control, [Candidate], Context, []).

-spec run(name(), fun(), fun() | list(fun()), any(), list()) -> any().
run(Name, Control, Candidates, Context, Options) when is_list(Candidates) ->
    case fuge_server:get(Name) of
        {ok, Fuge} ->
            fuge_run(Fuge, Control, Candidates, Context, Options);
        {error, not_found} ->
            Control()
    end;
run(Name, Control, Candidate, Context, Options) ->
    run(Name, Control, [Candidate], Context, Options).

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%% Check all the fuge options before the actual run
fuge_run(Fuge, Control, Candidates, Context, Options) ->
    case proplists:get_value(frequency, Fuge#fuge.options) of
        undefined ->
            do_run(Fuge, Control, Candidates, Context, Options);
        Value ->
            case Value >= random:uniform(100) of
                true ->
                    do_run(Fuge, Control, Candidates, Context, Options);
                false ->
                    Control()
            end
    end.

do_run(Fuge, Control, Candidates, Context, Options) ->
    NumberedCandidates = lists:zip(lists:seq(1, length(Candidates)), Candidates),
    Experiments = shuffle([{0, Control} | NumberedCandidates]),
    {Order, _} = lists:unzip(Experiments),
    RunExperiment = fun ({Id, Fun}) -> {Id, do_run_experiment(Fun, Options)} end,
    {_, [ControlR | CandidatesR]} =
        lists:unzip(lists:keysort(1, lists:map(RunExperiment, Experiments))),
    Result = #fuge_result{context = Context,
                          control = ControlR,
                          candidates = CandidatesR,
                          execution_order = Order},
    fuge_server:experiment(Fuge, Result),
    ControlR#fuge_data.value.

% TODO think and use sane options
do_run_experiment(Fun, _Options) ->
    Start = os:timestamp(),
    Result = Fun(),
    End = os:timestamp(),
    #fuge_data{duration = timer:now_diff(End, Start),
               value = Result}.

shuffle(List) ->
    [X || {_, X} <- lists:sort([{random:uniform(), E} || E <- List])].

%% Validate all options and return ones that failed
validate_options(Options) ->
    [Return || Return <-
               [validate_option(Option, Value) || {Option, Value} <- Options],
               Return =/= ok].

validate_option(frequency, Value)
  when is_integer(Value),
       Value > 0,
       Value =< 100 ->
    ok;
validate_option(Option, Value) ->
    {error, {invalid_option, {Option, Value}}}.
