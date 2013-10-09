%% Feel free to use, reuse and abuse the code in this file.

-module(http_uploader).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).

init({_Transport, http}, Req, []) ->
    {ok, Req, {}}.

handle(Req, State) ->
    {QsVals, Req2} = cowboy_req:qs_vals(Req),
    Opts = get_opts([
                     {max_files, 1},
                     {max_file_size, 1000},
                     {max_props, 10},
                     {max_prop_size, 5000}
                    ], QsVals),
    Opts2 = [{tmp_filename, fun(A) -> A end} | Opts],
    {Res, ReqR} = case ext_multipart:process(Req2, ext_multipart_default_handler,
                                             Opts2) of
                      {ok, Req3} ->
                          {Files, Req4} = cowboy_req:meta(files, Req3),
                          {Props, Req5} = cowboy_req:meta(props, Req4),
                          Result = {ok, [
                                         {files, Files},
                                         {props, Props}
                                        ]},
                          {Result, Req5};
                      {error, Reason, Req3} ->
                          Result = {error, Reason},
                          {Result, Req3}
                  end,
    {ok, ReqR2} = cowboy_req:reply(200, [], term_to_binary(Res), ReqR),
    {ok, ReqR2, State}.

terminate(_, _, _) ->
    ok.

get_opts(Opts, QsVals) ->
    [begin
         BinName = list_to_binary(atom_to_list(Name)),
         Value = case lists:keyfind(BinName, 1, QsVals) of
                     {_, V} -> binary_to_integer(V);
                     _ -> Default
                 end,
         {Name, Value}
     end || {Name, Default} <- Opts].
