-module(fuge_test).

-include_lib("eunit/include/eunit.hrl").

-export([control/0,
         candidate1/0,
         candidate2/0
        ]).

fuge_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      {"Basic test", fun fuge/0}
     ]}.

setup() ->
    application:start(fuge).

teardown(_) ->
    application:stop(fuge).

fuge() ->
    Fuge = fuge:new(test),
    fuge:run(test, fun ?MODULE:control/0, [fun ?MODULE:candidate1/0,
                                           fun ?MODULE:candidate2/0]),
    timer:sleep(100).

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
