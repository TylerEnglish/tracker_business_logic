-module(tracker_business_logic_app).

-behaviour(application).

%% API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", default_page_h, []},
            {"/package_transferred", package_transferred_handler, []},
            {"/delivered", delivered_handler, []},
            {"/location_request", location_request_handler, []},
            {"/location_update", location_update_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8081}], #{env => #{dispatch => Dispatch}}),
    tracker_business_logic_sup:start_link().

stop(_State) ->
    ok.
