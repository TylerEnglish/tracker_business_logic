-module(package_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-define(BASE_URL, "http://localhost:8081").

%% Test Cases
add_package_test_() ->
    {setup,
        fun() -> application:start(inets) end,
        fun(_) -> application:stop(inets) end,
        fun() ->
            [
                ?_test(add_package_success_test()),
                ?_test(add_package_invalid_ids_test())
            ]
        end}.

request_location_test_() ->
    {setup,
        fun() -> application:start(inets) end,
        fun(_) -> application:stop(inets) end,
        fun() ->
            [
                ?_test(request_location_success_test()),
                ?_test(request_location_nonexistent_package_test())
            ]
        end}.

mark_package_delivered_test_() ->
    {setup,
        fun() -> application:start(inets) end,
        fun(_) -> application:stop(inets) end,
        fun() ->
            [
                ?_test(mark_package_delivered_success_test()),
                ?_test(mark_package_delivered_nonexistent_package_test())
            ]
        end}.

update_location_test_() ->
    {setup,
        fun() -> application:start(inets) end,
        fun(_) -> application:stop(inets) end,
        fun() ->
            [
                ?_test(update_location_success_test()),
                ?_test(update_location_invalid_data_test())
            ]
        end}.

%% Add Package Tests
add_package_success_test() ->
    Request = {post, ?BASE_URL ++ "/package_transferred", [], "application/json",
               <<"{\"PackID\":123,\"LocID\":456}">>},
    {ok, {{_, 200, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"status\":\"ok\"}">>, Body).

add_package_invalid_ids_test() ->
    Request = {post, ?BASE_URL ++ "/package_transferred", [], "application/json",
               <<"{\"PackID\":\"invalid\",\"LocID\":\"invalid\"}">>},
    {ok, {{_, 400, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"error\":\"invalid_ids\"}">>, Body).

%% Request Location Tests
request_location_success_test() ->
    Request = {post, ?BASE_URL ++ "/location_request", [], "application/json",
               <<"{\"PackID\":123}">>},
    {ok, {{_, 200, _}, _, Body}} = httpc:request(Request),
    Expected = <<"{\"Lon\":34.5678,\"Lat\":-123.4567}">>,
    ?assertEqual(Expected, Body).

request_location_nonexistent_package_test() ->
    Request = {post, ?BASE_URL ++ "/location_request", [], "application/json",
               <<"{\"PackID\":999}">>},
    {ok, {{_, 404, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"error\":\"package_not_found\"}">>, Body).

%% Mark Package as Delivered Tests
mark_package_delivered_success_test() ->
    Request = {post, ?BASE_URL ++ "/delivered", [], "application/json",
               <<"{\"PackID\":123}">>},
    {ok, {{_, 200, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"status\":\"ok\"}">>, Body).

mark_package_delivered_nonexistent_package_test() ->
    Request = {post, ?BASE_URL ++ "/delivered", [], "application/json",
               <<"{\"PackID\":999}">>},
    {ok, {{_, 404, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"error\":\"package_not_found\"}">>, Body).

%% Update Location Tests
update_location_success_test() ->
    Request = {post, ?BASE_URL ++ "/location_update", [], "application/json",
               <<"{\"LocID\":456,\"Lon\":34.5678,\"Lat\":-123.4567}">>},
    {ok, {{_, 200, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"status\":\"ok\"}">>, Body).

update_location_invalid_data_test() ->
    Request = {post, ?BASE_URL ++ "/location_update", [], "application/json",
               <<"{\"LocID\":\"invalid\",\"Lon\":\"invalid\",\"Lat\":\"invalid\"}">>},
    {ok, {{_, 400, _}, _, Body}} = httpc:request(Request),
    ?assertEqual(<<"{\"error\":\"invalid_location\"}">>, Body).
