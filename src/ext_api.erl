-module(ext_api).

-export([init/2,
		 init/3,
         compile/1,
         execute/3]).

-record(?MODULE,
        {
          prefix,
          state
        }).

-record(route,
        {
          path,
          handler,
          routes,

          has_init=false,
          state,

          new,
          get,
          update,
          delete,

          find,

          hop,

          methods=[],
          g_methods=[],

          ctp,
          cta
        }).


-record(state,
        {
          dispatch=[],


          hops = [],
          target,
          route_env = [],

          id,
          operation,

          ctp,
          ctp_h,
          ctp_f,

          cta,

          on_error,

          data
        }).

%% Compiler

compile(Routes) ->
    [compile_route(Route) || Route <- Routes].

compile_route({Path, Handler, SubRoutes}) ->
    compile_route({Path, Handler, [], SubRoutes});
compile_route({Path, Handler, HandlerOpts, SubRoutes}) ->
    Route =
        #route{
           path = Path,
           handler = Handler,
           routes = compile(SubRoutes)
          },
    Funs = [
            fun(Rt) -> init_state(Rt, HandlerOpts) end,
            fun avaible_functions/1,
            fun avaible_methods/1,
            fun route_content_types_provided/1,
            fun route_content_types_accepted/1
           ],
    lists:foldl(fun(F, R) -> F(R) end, Route, Funs).

init_state(#route{handler=Handler} = Route, HandlerOpts) ->
    HasInit = function_exported(Handler, init, 1),
    case HasInit of
        true ->
            HandlerState = Handler:init(HandlerOpts),
            Route#route{has_init=true, state=HandlerState};
        false ->
            Route
    end.

avaible_functions(#route{has_init=HasInit, handler=Handler, state=HandlerState} = Route) ->
    GetFunction =
        case HasInit of
            true ->
                fun(M, F, A) ->
                        get_function(M, F, A + 1, HandlerState)
                end;
            false ->
                fun get_function/3
        end,
    Route#route{
      new = GetFunction(Handler, new, 3),
      get = GetFunction(Handler, get, 3),
      update = GetFunction(Handler, update, 4),
      delete = GetFunction(Handler, delete, 3),

      find = GetFunction(Handler, find, 2),

      hop = GetFunction(Handler, hop, 3)
     }.


avaible_methods(Route) ->
    Methods =
        [{<<"GET">>, Route#route.get},
         {<<"POST">>, Route#route.update},
         {<<"PUT">>, Route#route.update},
         {<<"DELETE">>, Route#route.delete}],
    GMethods =
        [{<<"GET">>, Route#route.find},
         {<<"POST">>, Route#route.new},
         {<<"PUT">>, Route#route.new}],
    Route#route{
       methods = binary_join(<<",">>, [M || {M, F} <- Methods, F =/= false]),
       g_methods = binary_join(<<",">>, [M || {M, F} <- GMethods, F =/= false])
      }.

route_content_types_provided(Route) ->
    case call_route(Route, content_types_provided, []) of
        {ok, CTP} ->
            CTP2 = [normalize_content_types(P) || P <- CTP],
            Route#route{ctp=CTP2};
        no_call ->
            Route#route{ctp=undefined}
    end.

route_content_types_accepted(Route) ->
    case call_route(Route, content_types_accepted, []) of
        {ok, CTA} ->
            CTA2 = [normalize_content_types(P) || P <- CTA],
            Route#route{cta=CTA2};
        no_call ->
            Route#route{cta=undefined}
    end.

%% Execute

init(Prefix, Dispatch) ->
    init(Prefix, Dispatch, []).
init(Prefix, Dispatch, Opts) ->
    #?MODULE{
        prefix = split_path(Prefix),
        state =
            #state{
               dispatch = ext_api:compile(Dispatch),
               ctp =  [normalize_content_types(P) || P <- get_value(content_types_provided, Opts, [])],
               cta = [normalize_content_types(P) || P <- get_value(content_types_accepted, Opts, [])],
               on_error = get_value(on_error, Opts, undefined)
              }
       }.

execute(Req, Env, #ext_api{prefix=Prefix, state=State}) ->
    {Path, Req2} = cowboy_req:path(Req),
    Path2 = split_path(Path),
    Len = length(Prefix),
    case catch lists:split(Len, Path2) of
        {Prefix, RestPath} ->
            match(RestPath, Req2, State);
        _ ->
            {ok, Req2, Env}
    end.

split_path(<< $/, Path/bits >>) ->
	split_path(Path, []);
split_path(_) ->
	badrequest.

split_path(Path, Acc) ->
	try
		case binary:match(Path, <<"/">>) of
			nomatch when Path =:= <<>> ->
				lists:reverse([cowboy_http:urldecode(S) || S <- Acc]);
			nomatch ->
				lists:reverse([cowboy_http:urldecode(S) || S <- [Path|Acc]]);
			{Pos, _} ->
				<< Segment:Pos/binary, _:8, Rest/bits >> = Path,
				split_path(Rest, [Segment|Acc])
		end
	catch
		error:badarg ->
			badrequest
	end.

match(RestPath, Req, #state{dispatch=Dispatch} = State) ->
    %% [_Host, Path] = cowboy_req:get([host, path], Req),
    case match_(Dispatch, RestPath, []) of
        {ok, [{Route, Id}|Rest]} ->
            State2 = State#state{
                       hops=lists:reverse(Rest),
                       target=Route,
                       id=Id
                      },
            choose_operation(Req, State2);
        {error, wrong_path = Reason} ->
            terminate_error(404, Reason, Req, State)
    end.

hop_test(_SubRoutes, _RestPath, [{#route{hop=false}, _},_|_]) ->
    {error, wrong_path};
hop_test(SubRoutes, RestPath, Acc) ->
    match_(SubRoutes, RestPath, Acc).

match_([#route{path=Path, routes=SubRoutes}=Route|_Rest], [Path|RestPath], Acc) ->
    case RestPath of
        [RouteID|RestPath2] ->
            hop_test(SubRoutes, RestPath2, [{Route, RouteID}|Acc]);
        _ ->
            hop_test(SubRoutes, RestPath, [{Route, undefined}|Acc])
    end;
match_([], [_|_], _) -> {error, wrong_path};
match_(_, [], []) -> {error, wrong_path};
match_(_, [], Acc)  -> {ok, Acc};
match_([_|_], [], _) -> {error, wrong_path};
match_([_|Rest], Path, Acc) -> match_(Rest, Path, Acc).

choose_operation(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    IsLast = State#state.id =:= undefined,
    Route = State#state.target,
    Succ = fun(Operation) ->
                   State2 = State#state{operation=Operation},
                   content_types_provided(Req, State2)
           end,
    case Method of
        <<"GET">> when IsLast andalso Route#route.find =/= false ->
            Succ(find);
        <<"GET">> when not IsLast andalso Route#route.get =/= false ->
            Succ(get);
        M when (M =:= <<"POST">> orelse M =:= <<"PUT">>)
			   andalso IsLast andalso Route#route.new =/= false ->
            Succ(new);
        M when (M =:= <<"POST">> orelse M =:= <<"PUT">>)
			   andalso not IsLast andalso Route#route.update =/= false ->
            Succ(update);
        <<"DELETE">> when not IsLast andalso Route#route.delete =/= false ->
            Succ(delete);
        _  ->
            AllowedMethods =
                case IsLast of
                    true ->
                        Route#route.g_methods;
                    false ->
                        Route#route.methods
                end,
            Req3 = cowboy_req:set_resp_header(<<"allow">>, AllowedMethods, Req2),
            terminate_error(405, Req3, State)
    end.

%% Content types provided

content_types_provided(Req, State) ->
    Route = State#state.target,
    State2 = case Route#route.ctp of
                 undefined ->
                     State;
                 Else ->
                     State#state{ctp = Else}
             end,
    content_types_provided(Req, State2, fun content_types_accepted/2).


content_types_provided(Req, State, Succ) ->
    case cowboy_req:parse_header(<<"accept">>, Req) of
        {error, badarg} ->
            terminate_error(400, Req, State);
        {ok, undefined, Req2} ->
            {PMT, Fun} = hd(State#state.ctp),
            State2 = State#state{
                       ctp_h = PMT,
                       ctp_f = Fun
                      },
            Succ(Req2, State2);
        {ok, Accept, Req2} ->
            Accept2 = prioritize_accept(Accept),
            choose_media_type(Req2, State, Accept2, Succ)
    end.

normalize_content_types({ContentType, Callback})
  when is_binary(ContentType) ->
    {cowboy_http:content_type(ContentType), Callback};
normalize_content_types(Normalized) ->
    Normalized.

prioritize_accept(Accept) ->
    lists:sort(
      fun ({MediaTypeA, Quality, _AcceptParamsA},
           {MediaTypeB, Quality, _AcceptParamsB}) ->
              %% Same quality, check precedence in more details.
              prioritize_mediatype(MediaTypeA, MediaTypeB);
          ({_MediaTypeA, QualityA, _AcceptParamsA},
           {_MediaTypeB, QualityB, _AcceptParamsB}) ->
              %% Just compare the quality.
              QualityA > QualityB
      end, Accept).

prioritize_mediatype({TypeA, SubTypeA, ParamsA}, {TypeB, SubTypeB, ParamsB}) ->
    case TypeB of
        TypeA ->
            case SubTypeB of
                SubTypeA -> length(ParamsA) > length(ParamsB);
                <<"*">> -> true;
                _Any -> false
            end;
        <<"*">> -> true;
        _Any -> false
    end.

choose_media_type(_Req, _State, [], _Succ) ->
    {error, 406, not_acceptable};
choose_media_type(Req, #state{ctp=CTP}=State, [MediaType|Tail], Succ) ->
    match_media_type(Req, State, Tail, CTP, MediaType, Succ).

match_media_type(Req, State, Accept, [], _MediaType, Succ) ->
    choose_media_type(Req, State, Accept, Succ);
match_media_type(Req, State, Accept, CTP,
                 MediaType = {{<<"*">>, <<"*">>, _Params_A}, _QA, _APA},
                 Succ) ->
    match_media_type_params(Req, State, Accept, CTP, MediaType, Succ);
match_media_type(Req, State, Accept,
                 CTP = [{{Type, SubType_P, _PP}, _Fun}|_Tail],
                 MediaType = {{Type, SubType_A, _PA}, _QA, _APA},
                 Succ)
  when SubType_P =:= SubType_A; SubType_A =:= <<"*">> ->
    match_media_type_params(Req, State, Accept, CTP, MediaType, Succ);
match_media_type(Req, State, Accept, [_Any|Tail], MediaType, Succ) ->
    match_media_type(Req, State, Accept, Tail, MediaType, Succ).

match_media_type_params(Req, State, _Accept,
                        [{{TP, STP, '*'}, Fun}|_Tail],
                        {{_TA, _STA, Params_A}, _QA, _APA},
                        Succ) ->
    PMT = {TP, STP, Params_A},
    State2 = State#state{
               ctp_h=PMT,
               ctp_f=Fun
              },
    Succ(Req, State2);
match_media_type_params(Req, State, Accept,
                        [{PMT = {_TP, _STP, Params_P}, Fun}|Tail],
                        MediaType = {{_TA, _STA, Params_A}, _QA, _APA},
                        Succ) ->
    case lists:sort(Params_P) =:= lists:sort(Params_A) of
        true ->
            State2 = State#state{
                       ctp_h=PMT,
                       ctp_f=Fun
                      },
            Succ(Req, State2);
        false ->
            match_media_type(Req, State, Accept, Tail, MediaType, Succ)
    end.

%% Content type accepted

content_types_accepted(Req, #state{operation=Operation} = State)
  when Operation =:= new orelse Operation =:= update ->
    Route = State#state.target,
    State2 = case Route#route.cta of
                 undefined ->
                     State;
                 Else ->
                     State#state{cta = Else}
             end,
    content_types_accepted(Req, State2, fun fold_routes/2);
content_types_accepted(Req, State) ->
    fold_routes(Req, State).

content_types_accepted(Req, State, Succ) ->
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, ContentType, Req2} ->
            choose_content_type(Req2, State, ContentType, State#state.cta, Succ);
        {error, badarg} ->
            terminate_error(415, Req, State)
    end.

choose_content_type(Req, State, _ContentType, [], _Succ) ->
    terminate_error(415, Req, State);
choose_content_type(Req, State, ContentType, [{Accepted, Fun}|_Tail], Succ)
  when Accepted =:= '*'; Accepted =:= ContentType ->
    process_data(Fun, Req, State, Succ);
choose_content_type(Req, State, {Type, SubType, Param},
                    [{{Type, SubType, AcceptedParam}, Fun}|_Tail], Succ)
  when AcceptedParam =:= '*'; AcceptedParam =:= Param ->
    process_data(Fun, Req, State, Succ);
choose_content_type(Req, State, ContentType, [_Any|Tail], Succ) ->
    choose_content_type(Req, State, ContentType, Tail, Succ).

process_data(Fun, Req, State, Succ) ->
    case Fun(Req) of
        {ok, Data, Req2} ->
            State2 = State#state{data=Data},
            Succ(Req2, State2);
        {error, Req2} ->
            terminate_error(415, Req2, State);
        {error, Reason, Req2} ->
            terminate_error(415, Reason, Req2, State)
    end.

%% Fold routes

fold_routes(Req, State) ->
    fold_routes(State#state.hops, Req, [],
                fun(RouteEnv, Req2) ->
                        State2 = State#state{
                                   route_env=RouteEnv
                                  },
                        process(Req2, State2)
                end).

fold_routes([], Req, RouteEnv, Succ) ->
    Succ(RouteEnv, Req);
fold_routes([{Route, Id}|Rest], Req, RouteEnv, Succ) ->
    case (Route#route.hop)(Req, Id, RouteEnv) of
        {ok, RouteEnv2, Req2} ->
            fold_routes(Rest, Req2, RouteEnv2, Succ);
        {error, _Reason, _Req} = Err ->
            Err
    end.

%% Process

process(Req, #state{operation=new, target=Route, route_env=RouteEnv, data=Data} = State) ->
    case (Route#route.new)(Req, Data, RouteEnv) of
        {ok, Result, Req2} ->
            reply(201, Result, Req2, State);
        {error, Reason, Req2} ->
            terminate_reason(Reason, Req2, State)
    end;
process(Req, #state{operation=get, id=Id, target=Route, route_env=RouteEnv} = State) ->
    case (Route#route.get)(Req, Id, RouteEnv) of
        {ok, Result, Req2} ->
            reply(200, Result, Req2, State);
        {error, Reason, Req2} ->
            terminate_reason(Reason, Req2, State)
    end;
process(Req, #state{operation=find, target=Route, route_env=RouteEnv} = State) ->
    case (Route#route.find)(Req, RouteEnv) of
        {ok, Result, Req2} ->
            reply(200, Result, Req2, State);
        {error, Reason, Req2} ->
            terminate_reason(Reason, Req2, State)
    end;
process(Req, #state{operation=update, id=Id, target=Route, data=Data, route_env=RouteEnv} = State) ->
    case (Route#route.update)(Req, Id, Data, RouteEnv) of
        {ok, Result, Req2} ->
            reply(200, Result, Req2, State);
        {error, Reason, Req2} ->
            terminate_reason(Reason, Req2, State)
    end;
process(Req, #state{operation=delete, id=Id, target=Route, route_env=RouteEnv} = State) ->
    case (Route#route.delete)(Req, Id, RouteEnv) of
        {ok, Result, Req2} ->
            reply(200, Result, Req2, State);
        {error, Reason, Req2} ->
            terminate_reason(Reason, Req2, State)
    end.

%% Reply

reply(Code, Result, Req, #state{ctp_f=Fun, ctp_h=PMT}) ->
    {ok, Body} = Fun(Result),
    {ok, Req2} = cowboy_req:reply(Code, [{<<"content-type">>, content_type(PMT)}], Body, Req),
    {halt, Req2}.

content_type({Type, SubType, Params}) ->
    ParamsBin = content_type_build_params(Params, []),
    [Type, <<"/">>, SubType, ParamsBin].

content_type_build_params('*', []) ->
    <<>>;
content_type_build_params([], []) ->
    <<>>;
content_type_build_params([], Acc) ->
    lists:reverse(Acc);
content_type_build_params([{Attr, Value}|Tail], Acc) ->
    content_type_build_params(Tail, [[Attr, <<"=">>, Value], <<";">>|Acc]).

%% ===================================================================
%%% Internal functions
%% ===================================================================

function_exported(Module, Function, Arity) ->
    lists:member({Function, Arity}, Module:module_info(exports)).

get_function(Module, Function, Arity) ->
    case function_exported(Module, Function, Arity) of
        true ->
            fun Module:Function/Arity;
        false ->
            false
    end.

get_function(Module, new, Arity, HandlerState) ->
    case function_exported(Module, new, Arity) of
        true ->
            fun(Req, Data, RouteEnv) ->
                    Module:new(Req, Data, HandlerState, RouteEnv)
            end;
        false ->
            false
    end;
get_function(Module, get, Arity, HandlerState) ->
    case function_exported(Module, get, Arity) of
        true ->
            fun(Req, Id, RouteEnv) ->
                    Module:get(Req, Id, HandlerState, RouteEnv)
            end;
        false ->
            false
    end;
get_function(Module, find, Arity, HandlerState) ->
    case function_exported(Module, find, Arity) of
        true ->
            fun(Req, RouteEnv) ->
                    Module:find(Req, HandlerState, RouteEnv)
            end;
        false ->
            false
    end;
get_function(Module, update, Arity, HandlerState) ->
    case function_exported(Module, update, Arity) of
        true ->
            fun(Req, Id, Data, RouteEnv) ->
                    Module:update(Req, Id, Data, HandlerState, RouteEnv)
            end;
        false ->
            false
    end;
get_function(Module, delete, Arity, HandlerState) ->
    case function_exported(Module, delete, Arity) of
        true ->
            fun(Req, Id, RouteEnv) ->
                    Module:delete(Req, Id, HandlerState, RouteEnv)
            end;
        false ->
           false
    end;
get_function(Module, hop, Arity, HandlerState) ->
    case function_exported(Module, hop, Arity) of
        true ->
            fun(Req, Id, RouteEnv) ->
                    Module:hop(Req, Id, HandlerState, RouteEnv)
            end;
        false ->
           false
    end.

get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        _ -> Default
    end.

binary_join(_Sep, []) ->
    <<>>;
binary_join(Sep, List) ->
    Size = byte_size(Sep),
    <<_:Size/binary, Rest/binary>> = << <<Sep/binary, Bin/binary>> || Bin <- List >>,
    Rest.

terminate_error(Code, Req, _State) ->
    {error, Code, Req}.

terminate_error(Code, _Reason, Req, _State) ->
    {error, Code, Req}.

terminate_reason(_Reason, Req, #state{on_error=undefined}) ->
    {error, 400, Req};
terminate_reason(Reason, Req, #state{on_error=OnError}=State) ->
    {Code, Body} = OnError(Reason),
    reply(Code, Body, Req, State).


call_route(#route{has_init=true, handler=Handler, state=HandlerState}, Fun, Args) ->
    call(Handler, Fun, Args ++ [HandlerState]);
call_route(#route{handler=Handler}, Fun, Args) ->
    call(Handler, Fun, Args).

call(M, F, Args) ->
    Arity = length(Args),
    case function_exported(M, F, length(Args)) of
        true ->
            {ok, apply(fun M:F/Arity, Args)};
        false ->
            no_call
    end.
