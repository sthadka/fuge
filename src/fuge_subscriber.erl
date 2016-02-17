%%%-------------------------------------------------------------------
%% @doc fuge subscriber behaviour
%% @end
%%%-------------------------------------------------------------------

-module(fuge_subscriber).

-include("fuge.hrl").

-callback init(Fuge :: fuge:fuge(),
               State :: fuge:subscriber_state()) ->
    NewState :: fuge:subscriber_state().

-callback handle_result(Fuge :: fuge:fuge(),
                        State :: fuge:subscriber_state(),
                        Result :: fuge:result()) ->
    ok.
