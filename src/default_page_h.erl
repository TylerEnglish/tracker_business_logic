-module(default_page_h).
-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, to_html/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"text/html">>, to_html}], Req, State}.

to_html(Req, State) ->
    %% Debugging: log the request
    io:format("Request: ~p~n", [Req]),
    %% Reply with a 200 status code and "Hello, world!" message
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, <<"Hello, world!">>, Req),
    {stop, Req2, State}.
