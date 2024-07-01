-module(tracker_business_logic).

-export([start/0, stop/0]).

start() ->
    tracker_business_logic_sup:start_link().

stop() ->
    ok.
