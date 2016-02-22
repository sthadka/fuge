-module(fuge_test).

-include_lib("eunit/include/eunit.hrl").

-export([control/0,
         candidate1/0,
         candidate2/0
        ]).

fuge_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      {"Simple run",       fun simple_run/0},
      {"Frequency option", fun frequency_option/0}
     ]}.

setup() ->
    application:start(fuge).

teardown(_) ->
    application:stop(fuge).

simple_run() ->
    ok = fuge:new(simple_run),
    fuge:run(test, fun ?MODULE:control/0, fun ?MODULE:candidate1/0),
    fuge:run(test, fun ?MODULE:control/0, [fun ?MODULE:candidate1/0,
                                           fun ?MODULE:candidate2/0]),
    timer:sleep(100).

frequency_option() ->
    Subscribers = [],
    Frequency = 42,
    Options = [{frequency, Frequency}],
    ErrorMargin = 0.1,
    NumRuns = 1000,

    ok = fuge:new(frequency_option, Subscribers, Options),

    ControlTable = ets:new(control, []),
    ets:insert(ControlTable, {count, 0}),
    CandidateTable = ets:new(candidate, []),
    ets:insert(CandidateTable, {count, 0}),
    Control = fun() -> ets:update_counter(ControlTable, count, 1) end,
    Candidate = fun() -> ets:update_counter(CandidateTable, count, 1) end,

    lists:foreach(fun (_) ->
                          fuge:run(frequency_option, Control, Candidate)
                  end, lists:seq(1, NumRuns)),

    ?assert(((NumRuns * ((Frequency - Frequency * ErrorMargin) / 100)) <
             ets:lookup_element(CandidateTable, count, 2))),

    ?assert(ets:lookup_element(CandidateTable, count, 2) <
            (NumRuns * ((Frequency + Frequency * ErrorMargin) / 100))).

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

%% Control function calculates sum of all numbers between 1, 1000 using
%% functions from standard library
control() ->
    lists:sum(lists:seq(1, 1000)).

%% Candidate function calculates sum of all numbers between 1, 1000 using
%% custom function code
candidate1() ->
    sum(1, 1000).

%% Candidate function calculates sum of all numbers between 1, 1000 using
%% the formula -> n * (n + 1) / 2
candidate2() ->
    1000 * (1000 + 1) div 2.

sum(Start, End) ->
    sum(Start, End, 0).

sum(End, End, Sum) ->
    End + Sum;
sum(Start, End, Sum) ->
    sum(Start+1, End, Start + Sum).
