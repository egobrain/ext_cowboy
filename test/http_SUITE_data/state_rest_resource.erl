-module(state_rest_resource).

-export([
         init/1,
         hop/4,
         new/4,
         get/4,
         update/5,
         delete/4,
         find/3,
         delete/3
        ]).

init(Opts) ->
    Opts.

hop(Req, Id, State, Env) ->
    {ok, [{{?MODULE, State}, hop, Id}|Env], Req}.

new(Req, Data, State, Env) ->
    {ok, {{?MODULE, State}, new, Data, Env}, Req}.

get(Req, Id, State, Env) ->
    {ok, {{?MODULE, State}, get, Id, Env}, Req}.

update(Req, Id, Data, State, Env) ->
    {ok, {{?MODULE, State}, update, Id, Data, Env}, Req}.

delete(Req, Id, State, Env) ->
    {ok, {{?MODULE, State}, delete, Id, Env}, Req}.

find(Req, State, Env) ->
    {ok, {{?MODULE, State}, find, Env}, Req}.

delete(Req, State, Env) ->
    {ok, {{?MODULE, State}, g_delete, Env}, Req}.
