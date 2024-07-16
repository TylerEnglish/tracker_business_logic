-module(simple_http_handler).
-behaviour(cowboy_rest).

-export([init/2]).

init(Req, _State) ->
    io:format("simple_http_handler called~n"),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello, world!">>, Req),
    {ok, Req2, #{}}.