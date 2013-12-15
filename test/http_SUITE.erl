%% Copyright (c) 2013, Egobrain <xazar.studio@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(http_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2
        ]).

%% Tests.
-export([
         multipart_file/1,
         multipart_files/1,
         multipart_big_file_error/1,
         multipart_files_error/1,

         multipart_prop/1,
         multipart_props/1,
         multipart_big_prop_error/1,
         multipart_props_error/1,

         regexp_router_simple/1,
         regexp_router_any/1,
         regexp_router_data/1,
         regexp_router_int_re/1,
         regexp_router_int_constr/1,
         regexp_router_int_fun/1,

         api_simple/1,
		 api_state/1,
		 api_child/1,
         api_state_child/1
        ]).

all() ->
    [
     {group, http},
     {group, regexp_router},
     {group, rest_api}
    ].

groups() ->
    Tests =
        [
         multipart_file,
         multipart_files,
         multipart_big_file_error,
         multipart_files_error,

         multipart_prop,
         multipart_props,
         multipart_big_prop_error,
         multipart_props_error
        ],

    RegexpRouterTests =
        [
         regexp_router_simple,
         regexp_router_any,
         regexp_router_data,
         regexp_router_int_re,
         regexp_router_int_constr,
         regexp_router_int_fun
        ],
    RestApiTests =
        [
         api_simple,
		 api_state,
		 api_child,
         api_state_child
        ],
    [
     {http, [parallel], Tests},
     {regexp_router, [parallel], RegexpRouterTests},
     {rest_api, [parallel], RestApiTests}
    ].

init_per_suite(Config) ->
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),
    Config.

end_per_suite(_Config) ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(cowlib),
    application:stop(crypto),
    ok.

init_per_group(http, Config) ->
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(http, 100, [{port, 0}],
                                [
                                 {env, [{dispatch, init_dispatch(Config)}]},
                                 {max_keepalive, 50},
                                 {timeout, 500}
                                ]),
    Port = ranch:get_port(http),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>}, {port, Port}, {opts, []},
     {transport, Transport}, {client, Client}|Config];

init_per_group(regexp_router = Name, Config) ->
    Transport = ranch_tcp,
    {ok, _} = cowboy:start_http(Name, 100, [{port, 0}],
                                [
                                 {env, [{regexp_dispatch, init_regexp_dispatch(Config)}]},
                                 {middlewares, [ext_regexp_router, cowboy_handler]},
                                 {max_keepalive, 50},
                                 {timeout, 500}
                                ]),
    Port = ranch:get_port(Name),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>}, {port, Port}, {opts, []},
     {transport, Transport}, {client, Client}|Config];
init_per_group(rest_api = Name, Config) ->
    Transport = ranch_tcp,
	Opts = [
			{content_types_provided,
			 [
			  {<<"application/bert">>, fun to_bert/1}
			 ]},
			{content_types_accepted,
			 [
			  {<<"application/bert">>, fun from_bert/1}
			 ]}
		   ],
    {ok, _} = cowboy:start_http(Name, 100, [{port, 0}],
                                [
                                 {env, []},
                                 {middlewares,
                                  [
                                   ext_api:init(<<"/api/v1">>, init_api_dispatch(Config), Opts),
                                   cowboy_handler]},
                                 {max_keepalive, 50},
                                 {timeout, 500}
                                ]),
    Port = ranch:get_port(Name),
    {ok, Client} = cowboy_client:init([]),
    [{scheme, <<"http">>}, {port, Port}, {opts, []},
     {transport, Transport}, {client, Client}|Config].

end_per_group(Name, _) ->
    cowboy:stop_listener(Name),
    ok.

%% Dispatch configuration.

init_dispatch(_Config) ->
    cowboy_router:compile(
      [
       {"localhost",
        [
         {"/uploader", http_uploader, []}
        ]}
      ]).

init_regexp_dispatch(_Config) ->
    ext_regexp_router:compile(
      [
       {".*",
        [
         {"/simple", regexp_router_handler, <<"simple">>},
         {"/[any]+", regexp_router_handler, <<"any">>},
         {"/data/(?<data>.+)", regexp_router_handler, <<"data">>},
         {"/int_re/(?<data>\\d+)", regexp_router_handler, <<"int_re">>},
         {"/int_constr/(?<data>.+)", [{data, int}], regexp_router_handler, <<"int_constr">>},
         {"/int_fun/(?<data>.+)", [{data, function, fun bin_to_int/1}],
          regexp_router_handler, <<"int_fun">>}
        ]}
      ]).

init_api_dispatch(_Config) ->
    [
     {<<"simple">>, rest_resource, []},
     {<<"state">>, state_rest_resource, state, []},
     {<<"parent">>, rest_resource,
      [
       {<<"child">>, rest_resource, []}
      ]},
     {<<"state_parent">>, state_rest_resource, state,
      [
       {<<"state_child">>, state_rest_resource, state, []}
      ]}
    ].



%% Convenience functions.

build_url(Path, Config) ->
    {scheme, Scheme} = lists:keyfind(scheme, 1, Config),
    {port, Port} = lists:keyfind(port, 1, Config),
    PortBin = list_to_binary(integer_to_list(Port)),
    PathBin = list_to_binary(Path),
    << Scheme/binary, "://localhost:", PortBin/binary, PathBin/binary >>.

%% Tests.

multipart_file(Config) ->
    Body = gen_binary(1000),
    Parts = [
             {[{<<"content-disposition">>,
                <<"form-data; name=\"f1\"; filename=\"file1.raw\"">>},
               {<<"content-type">>, <<"application/octet-stream">>}],
              Body}
            ],
    Url = "/uploader?max_file_size=1000&max_files=1",
    {ok, Res} = multipart_test(Config, Url, Parts),
    [
     {files,
      [
       {<<"f1">>, [
                   {path, Path},
                   {size, 1000},
                   {filename, <<"file1.raw">>},
                   {content_type, <<"application/octet-stream">>}
                  ]}
      ]},
     {props,[]}
    ] = Res,
    {ok, Body} = file:read_file(Path).

multipart_files(Config) ->
    Body1 = gen_binary(1000),
    Body2 = gen_binary(2000),
    Parts = [
             {[{<<"content-disposition">>,
                <<"form-data; name=\"fm1\"; filename=\"file-m1.raw\"">>},
               {<<"content-type">>, <<"application/octet-stream">>}],
              Body1},
             {[{<<"content-disposition">>,
                <<"form-data; name=\"fm2\"; filename=\"file-m2.raw\"">>},
               {<<"content-type">>, <<"application/octet-stream">>}],
              Body2}
            ],
    Url = "/uploader?max_file_size=2000&max_files=2",
    {ok, Res} = multipart_test(Config, Url, Parts),
    [
     {files,
      [
       {<<"fm1">>, [
                    {path, Path1},
                    {size, 1000},
                    {filename, <<"file-m1.raw">>},
                    {content_type, <<"application/octet-stream">>}
                   ]},
       {<<"fm2">>, [
                    {path, Path2},
                    {size, 2000},
                    {filename, <<"file-m2.raw">>},
                    {content_type, <<"application/octet-stream">>}
                   ]}
      ]},
     {props,[]}
    ] = Res,
    {ok, Body1} = file:read_file(Path1),
    {ok, Body2} = file:read_file(Path2).

multipart_big_file_error(Config) ->
    Parts = [
             {[{<<"content-disposition">>,
                <<"form-data; name=\"fe1\"; filename=\"file-e1.raw\"">>},
               {<<"content-type">>, <<"application/octet-stream">>}],
              gen_binary(1000)}
            ],
    Url = "/uploader?max_file_size=999&max_files=1",
    {error, {<<"fe1">>, {max_size, 999}}} = multipart_test(Config, Url, Parts).

multipart_files_error(Config) ->
    Parts = [
             {[{<<"content-disposition">>,
                <<"form-data; name=\"fme1\"; filename=\"file-me1.raw\"">>},
               {<<"content-type">>, <<"application/octet-stream">>}],
              gen_binary(1000)},
             {[{<<"content-disposition">>,
                <<"form-data; name=\"fme2\"; filename=\"file-me2.raw\"">>},
               {<<"content-type">>, <<"application/octet-stream">>}],
              gen_binary(1000)}
            ],
    Url = "/uploader?max_file_size=1000&max_files=1",
    {error, {max_files, 1}} = multipart_test(Config, Url, Parts).

multipart_prop(Config) ->
    Parts = [
             {[{<<"content-disposition">>, <<"form-data; name=\"p1\"">>}],
              <<"property_1">>}
            ],
    Url = "/uploader?max_prop_size=5000&max_props=1",
    {ok, Res} = multipart_test(Config, Url, Parts),
    [
     {files,[]},
     {props,[
             {<<"p1">>, <<"property_1">>}
            ]}
    ] = Res.

multipart_props(Config) ->
    Parts = [
             {[{<<"content-disposition">>, <<"form-data; name=\"pm1\"">>}],
              <<"property_1">>},
             {[{<<"content-disposition">>, <<"form-data; name=\"pm2\"">>}],
              <<"property_2">>}
            ],
    Url = "/uploader?max_prop_size=5000&max_props=2",
    {ok, Res} = multipart_test(Config, Url, Parts),
    [
     {files,[]},
     {props,[
             {<<"pm1">>, <<"property_1">>},
             {<<"pm2">>, <<"property_2">>}
            ]}
    ] = Res.

multipart_big_prop_error(Config) ->
    Parts = [
             {[{<<"content-disposition">>, <<"form-data; name=\"pe1\"">>}],
              <<"012345678910">>}
            ],
    Url = "/uploader?max_prop_size=10&max_props=1",
    {error, Reason} = multipart_test(Config, Url, Parts),
    {<<"pe1">>, {max_size, 10}} = Reason.

multipart_props_error(Config) ->
    Parts = [
             {[{<<"content-disposition">>, <<"form-data; name=\"pme1\"">>}],
              <<"property_1">>},
             {[{<<"content-disposition">>, <<"form-data; name=\"pme2\"">>}],
              <<"property_2">>}
            ],
    Url = "/uploader?max_prop_size=5000&max_props=1",
    {error, Reason} = multipart_test(Config, Url, Parts),
    {max_props, 1} = Reason.

multipart_test(Config, Url, Parts) ->
    Client = ?config(client, Config),
    Boundry = <<"--OHai">>,
    Body = join_parts(Parts, Boundry),
    %% ct:pal("Body:~n~s~n", [Body]),
    {ok, Client2} = cowboy_client:request(
                      <<"POST">>,
                      build_url(Url, Config),
                      [{<<"content-type">>,
                        <<"multipart/form-data; boundary=",
                          Boundry/binary>>}
                      ],
                      Body, Client),
    {ok, 200, _, Client3} = cowboy_client:response(Client2),
    {ok, RespBody, _} = cowboy_client:response_body(Client3),
    binary_to_term(RespBody).

% Regexp router tests

regexp_router_simple(Config) ->
    {ok, <<"simple">>, []} = regexp_router_test("/simple", Config).

regexp_router_any(Config) ->
    {ok, <<"any">>, []} = regexp_router_test("/any", Config),
    {ok, <<"any">>, []} = regexp_router_test("/aaa", Config),
    {ok, <<"any">>, []} = regexp_router_test("/annnnny", Config).

regexp_router_data(Config) ->
    {ok, <<"data">>, [{data, <<"123">>}]} = regexp_router_test("/data/123", Config).

regexp_router_int_re(Config) ->
    {ok, <<"int_re">>, [{data, <<"123">>}]} = regexp_router_test("/int_re/123", Config).

regexp_router_int_constr(Config) ->
    {ok, <<"int_constr">>, [{data, 123}]} = regexp_router_test("/int_constr/123", Config).

regexp_router_int_fun(Config) ->
    {ok, <<"int_fun">>, [{data, 123}]} = regexp_router_test("/int_fun/123", Config).

regexp_router_test(Url, Config) ->
    Client = ?config(client, Config),
    {ok, Client2} = cowboy_client:request(<<"GET">>, build_url(Url, Config), Client),
    {ok, 200, _, Client3} = cowboy_client:response(Client2),
    {ok, RespBody, _} = cowboy_client:response_body(Client3),
    binary_to_term(RespBody).

% API tests
api_simple(Config) ->
    Data = api_simple,
    Urls = [
            {"simple", rest_resource, "id"}
           ],
    test_api(Data, Urls, Config).

api_state(Config) ->
    Data = api_state,
    Urls = [
            {"state", {state_rest_resource, state}, "id"}
           ],
    test_api(Data, Urls, Config).

api_child(Config) ->
	Data = api_child,
    Urls = [
            {"parent", rest_resource, "id1"},
            {"child", rest_resource, "id2"}
           ],
    test_api(Data, Urls, Config).


api_state_child(Config) ->
    Data = api_state_child,
    Urls = [
            {"state_parent", {state_rest_resource, state}, "id1"},
            {"state_child", {state_rest_resource, state}, "id2"}
           ],
    test_api(Data, Urls, Config).

test_api(Data, UrlsList, Config) ->
    Tests = api_tests(Data, UrlsList),

    [Result = api_test(Url, Method, Data, Config)
     || {Result, Url, Method} <- Tests].

    %% [
    %%  try
    %%      R1 = api_test(Url, Method, Data, Config),
    %%      ct:pal("~p ~n<>~n ~p ~n ~p ~p~n", [R1, Result, Url, Method]),
    %%      R1 = Result
    %%  catch E:R ->
    %%          ct:pal("~p:~p ~p ~p", [E, R, Url, Method])
    %%  end || {Result, Url, Method} <- Tests].

api_tests(Data, UrlsList) ->
    Ops = [find, new, get, update, delete, g_delete],
    lists:flatten(
      [api_tests(Op, Data, UrlsList) || Op <- Ops]).
api_tests(Op, Data, UrlsList) ->
    Prefix = "/api/v1",
    Len = length(UrlsList),
    {H, [T]} = lists:split(Len-1, UrlsList),
    ParentUrl = [ U++"/"++Id || {U, _, Id} <- H],
    State = [{Module, hop, list_to_binary(Id)} || {_, Module, Id} <- H],
    ParentUrl2 = string:join([Prefix|ParentUrl], "/"),
    Url = foldUrl(ParentUrl2, Op, T),
    Methods = methods(Op),
    Result = {code(Op), op_data(Op, T, Data, State)},
    [{Result, Url, Method} || Method <- Methods].

foldUrl(Prefix, Op, {Uri, _, _Id})
  when Op =:= new orelse
       Op =:= find orelse
       Op =:= g_delete ->
    string:join([Prefix, Uri], "/");
foldUrl(Prefix, Op, {Uri, _, Id})
  when Op =:= get orelse
       Op =:= update orelse
       Op =:= delete ->
    string:join([Prefix, Uri, Id], "/").

op_data(find = Op, {_, Module, _Id}, _Data, State) ->
    {Module, Op, State};
op_data(get = Op, {_, Module, Id}, _Data, State) ->
    {Module, Op, list_to_binary(Id), State};
op_data(new = Op, {_, Module, _Id}, Data, State) ->
    {Module, Op, Data, State};
op_data(update = Op, {_, Module, Id}, Data, State) ->
    {Module, Op, list_to_binary(Id), Data, State};
op_data(delete = Op, {_, Module, Id}, _Data, State) ->
    {Module, Op, list_to_binary(Id), State};
op_data(g_delete = Op, {_, Module, _Id}, _Data, State) ->
    {Module, Op, State}.

code(new) -> 201;
code(_) -> 200.

methods(Op)
  when Op =:= get orelse
       Op =:= find ->
    [<<"GET">>];
methods(Op)
  when Op =:= new orelse
       Op =:= update ->
    [<<"POST">>, <<"PUT">>];
methods(Op)
  when Op =:= delete orelse
       Op =:= g_delete ->
    [<<"DELETE">>].

api_test(Url, Method, Data, Config) ->
    Url2 = build_url(Url, Config),
    Client = ?config(client, Config),
    Headers =
        [
         {<<"accept">>, <<"application/bert">>},
         {<<"content-type">>, <<"application/bert">>}
        ],
    {ok, Client2} =
        case Method =:= <<"POST">> orelse Method =:= <<"PUT">> of
            true ->
                Data2 = term_to_binary(Data),
                cowboy_client:request(Method, Url2, Headers, Data2, Client);
            false ->
                cowboy_client:request(Method, Url2, Headers, Client)
        end,
    {ok, Code, _, Client3} = cowboy_client:response(Client2),
    try
		{ok, RespBody, _} = cowboy_client:response_body(Client3),
		{Code, binary_to_term(RespBody)}
	catch _:_ ->
			Code
	end.

% Ineternal functions

gen_binary(Bytes) ->
    crypto:strong_rand_bytes(Bytes).

join_parts(Parts, Boundry) ->
    binary:list_to_bin(
      [
       [
        <<"\r\n--", Boundry/binary>>,
        << <<"\r\n", H/binary, ": ", V/binary>> || {H, V} <- Headers>>,
        <<"\r\n\r\n", Body/binary>>
       ] || {Headers, Body} <- Parts
      ] ++ [<<"\r\n--", Boundry/binary, "--\r\n">>]).

bin_to_int(Bin) ->
    try {true, list_to_integer(binary_to_list(Bin))}
    catch _:_ -> false
    end.

from_bert(Req) ->
	{ok, Body, Req2} = cowboy_req:body(Req),
	Data = binary_to_term(Body),
	{ok, Data, Req2}.

to_bert(Data) ->
	Binary = term_to_binary(Data),
	{ok, Binary}.
