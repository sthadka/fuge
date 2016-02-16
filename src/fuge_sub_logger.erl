-module(fuge_sub_logger).

-include("fuge.hrl").

-export([init/2,
         handle_result/3]).

init(_Fuge, State) ->
    State.

handle_result(Fuge, _State, Result) ->
    error_logger:info_msg("Fuge: ~p", [Fuge]),
    error_logger:info_msg("Result: ~p", [Result]).
