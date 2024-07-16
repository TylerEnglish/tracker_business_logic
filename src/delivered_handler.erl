-module(delivered_handler).
-behaviour(cowboy_rest).

%% API
-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2, is_authorized/2, handle_post/2, to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, handle_post}], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"text/html">>, fun to_html/2}], Req, State}.

handle_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    io:format("Received Body: ~s~n", [Body]),
    Json = jsx:decode(Body, [return_maps]),
    PackID = maps:get(<<"PackID">>, Json),
    io:format("Decoded JSON: PackID=~p~n", [PackID]),
    Result = package_server:delivered(PackID),
    io:format("Result from package_server: ~p~n", [Result]),
    Req3 = case Result of
        ok ->
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"{\"status\":\"success\"}">>, Req2);
        {error, Reason} ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, io_lib:format("{\"error\":\"~p\"}", [Reason]), Req2);
        _ ->
            cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"internal_error\"}">>, Req2)
    end,
    {stop, Req3, State}.

to_html(Req, State) ->
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, <<"Delivered Handler">>, Req),
    {stop, <<"Delivered Handler">>, Req2, State}.
