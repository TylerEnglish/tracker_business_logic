-module(tracker_business_logic_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = application:ensure_all_started(goldrush),
    {ok, _} = application:ensure_all_started(lager),
    tracker_business_logic_sup:start_link().

stop(_State) ->
    ok.
