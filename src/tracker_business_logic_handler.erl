% src/tracker_business_logic_handler.erl
-module(tracker_business_logic_handler).
-behaviour(cowboy_rest).

-export([init/2, allowed_methods/2, content_types_provided/2, to_html/2, terminate/3]).

init(Req, State) ->
    io:format("init called~n"),
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    io:format("allowed_methods called~n"),
    {["GET"], Req, State}.

content_types_provided(Req, State) ->
    io:format("content_types_provided called~n"),
    {[{"text/html", to_html}], Req, State}.

to_html(Req, State) ->
    io:format("to_html called~n"),
    {ok, <<"Hello, world!">>, Req, State}.

terminate(_Reason, _Req, _State) ->
    io:format("terminate called~n"),
    ok.
