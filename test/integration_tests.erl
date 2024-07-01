-module(integration_tests).
-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
    {setup,
     fun() -> application:start(tracker_business_logic) end,
     fun(_State) -> application:stop(tracker_business_logic) end,
     fun() ->
         %% Verify application has started
         ?_assertEqual({ok, started}, application:which_applications())
     end}.
