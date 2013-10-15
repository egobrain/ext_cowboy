-module(rest_resource).

-export([
         hop/3,
         new/3,
         get/3,
         update/4,
         delete/3,
         find/2
        ]).

hop(Req, Id, Env) ->
    {ok, [{?MODULE, hop, Id}|Env], Req}.

new(Req, Data, Env) ->
    {ok, {?MODULE, new, Data, Env}, Req}.

get(Req, Id, Env) ->
    {ok, {?MODULE, get, Id, Env}, Req}.

update(Req, Id, Data, Env) ->
    {ok, {?MODULE, update, Id, Data, Env}, Req}.

delete(Req, Id, Env) ->
    {ok, {?MODULE, delete, Id, Env}, Req}.

find(Req, Env) ->
    {ok, {?MODULE, find, Env}, Req}.
