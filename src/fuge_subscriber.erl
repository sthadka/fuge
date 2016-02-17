%%%-------------------------------------------------------------------
%% @doc fuge subscriber behaviour
%% @end
%%%-------------------------------------------------------------------

-module(fuge_subscriber).

-include("fuge.hrl").

-callback init(Fuge :: fuge(),
               State :: subscriber_state()) ->
    NewState :: subscriber_state().

-callback handle_result(Fuge :: fuge(),
                        State :: subscriber_state(),
                        Result :: result()) ->
    ok.
