%% Feel free to use, reuse and abuse the code in this file.

-module(regexp_router_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Transport, http}, Req, Data) ->
    {ok, Req, Data}.

handle(Req, Data) ->
    {Bindings, Req2} = cowboy_req:bindings(Req),
    Result = {ok, Data, Bindings},
    {ok, Req3} = cowboy_req:reply(200, [], term_to_binary(Result), Req2),
    {ok, Req3, Data}.

terminate(_, _, _) ->
    ok.
