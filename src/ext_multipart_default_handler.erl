-module(ext_multipart_default_handler).

-behavior(ext_multipart_handler).

-export([
         init/1,
         start_of_part/2,
         part_data/2,
         end_of_part/1,
         finish/1,
         terminate/2
        ]).

-define(MAX_PROP_SIZE, 5000).
-define(MAX_PROPS, 10).
-define(MAX_FILE_SIZE, 5000).
-define(MAX_FILES, unlimited).
-define(TMP_PATH, "/tmp").
-define(TMP_FILE_NAME, fun tmp_filename/1).

-record(state,{
          files = [],
          props = [],

          name,

          file_opts,
          buff = <<>>,

          max_file_size :: non_neg_integer(),
          max_files :: non_neg_integer() | unlimited,
          tmp_folder :: string(),
          tmp_filename :: function(),
          max_prop_size :: non_neg_integer(),
          max_props :: non_neg_integer()
         }).

-record(file_opts, {
          file,
          path,
          size,
          filename,
          content_type
         }).

init(Opts) ->
    #state{
       max_files = proplists:get_value(max_files, Opts, ?MAX_FILES),
       tmp_folder = proplists:get_value(tmp_folder, Opts, ?TMP_PATH),
       tmp_filename = proplists:get_value(tmp_filename, Opts, ?TMP_FILE_NAME),
       max_file_size = proplists:get_value(max_file_size, Opts, ?MAX_FILE_SIZE),
       max_prop_size = proplists:get_value(max_prop_size, Opts, ?MAX_PROP_SIZE),
       max_props = proplists:get_value(max_props, Opts, ?MAX_PROPS)
      }.

% start of part

start_of_part(Headers, State) ->
    {_, Disp} = lists:keyfind(<<"content-disposition">>, 1, Headers),
    {_DispToken, DispParams} = cowboy_multipart:content_disposition(Disp),
    {_, Name} = lists:keyfind(<<"name">>, 1, DispParams),
    State2 = State#state{name=Name},
    IsFile = lists:keymember(<<"filename">>, 1, DispParams),
    case IsFile of
        true ->
            ContentType =
                case lists:keyfind(<<"content-type">>, 1, Headers) of
                    {_, Ct} -> Ct;
                    false -> <<"application/octet-stream">>
                end,
            start_file(ContentType, DispParams, State2);
        false ->
            start_prop(State2)
    end.

start_file(_ContentType, _DispParams, #state{
                                         max_files=MaxFiles,
                                         files=Files
                                        }) when length(Files) >= MaxFiles ->
    {error, {max_files, MaxFiles}};
start_file(ContentType, DispParams, #state{
                                       name=Name,
                                       tmp_folder=TmpFolder,
                                       tmp_filename=TmpFilenameFun
                                      }=State) ->
    TmpPath = filename:join(TmpFolder, TmpFilenameFun(Name)),
    case file:open(TmpPath, [raw, write]) of
        {ok, File} ->
            {_, Filename} = lists:keyfind(<<"filename">>, 1, DispParams),
            FileOpts = #file_opts{
                          file=File,
                          path=TmpPath,
                          size=0,
                          filename=Filename,
                          content_type=ContentType
                         },
            State2 = State#state{
                       file_opts=FileOpts
                      },
            {ok, State2};
        {error, _Reason} = Err ->
            Err
    end.

start_prop(#state{
              max_props=MaxProps,
              props=Props
             }) when length(Props) >= MaxProps ->
    {error, {max_props, MaxProps}};
start_prop(State) ->
    State2 = State#state{
               buff = <<>>
              },
    {ok, State2}.

part_data(Data, #state{
                   name=Name,
                   file_opts=undefined,
                   buff=Buff,
                   max_prop_size=MaxPropSize
                  } = State) ->
    case prop_data(Data, MaxPropSize, Buff) of
        {ok, Buff2} ->
            State2 = State#state{
                       buff=Buff2
                      },
            {ok, State2};
        {error, Reason} ->
            {error, {Name, Reason}}
    end;
part_data(Data, #state{
                   name=Name,
                   file_opts=FileOpts,
                   max_file_size=MaxFileSize
                  } = State) ->
    case file_data(Data, MaxFileSize, FileOpts) of
        {ok, FileOpts2} ->
            State2 = State#state{
                       file_opts=FileOpts2
                      },
            {ok, State2};
        {error, Reason} ->
            {error, {Name, Reason}}
    end.

file_data(Data, MaxFileSize, #file_opts{
                                file=File,
                                size=Size
                               }=FileOpts) ->
    NewSize = Size + byte_size(Data),
    case NewSize > MaxFileSize of
        true ->
            {error, {max_size, MaxFileSize}};
        false ->
            ok = file:write(File,Data),
            FileOpts2 = FileOpts#file_opts{
                          size=NewSize
                         },
            {ok, FileOpts2}
    end.

prop_data(Data, MaxPropSize, Buff)
  when byte_size(Data) + byte_size(Buff) >= MaxPropSize ->
    {error, {max_size, MaxPropSize}};
prop_data(Data, _MaxPropSize, Buff) ->
    {ok, <<Buff/binary, Data/binary>>}.

end_of_part(#state{
               file_opts=undefined,
               props=Props,
               name=Name,
               buff=Buff}=State) ->
    State2 = State#state{
               props = [{Name, Buff}|Props],
               buff = <<>>
              },
    {ok, State2};
end_of_part(#state{
               files=Files,
               name=Name,
               file_opts=FileOpts}=State) ->
    #file_opts{
       file = File,
       path = Path,
       size = Size,
       filename = Filename,
       content_type = ContentType
      } = FileOpts,
    Proplist = [
                {path, Path},
                {size, Size},
                {filename, Filename},
                {content_type, ContentType}
               ],
    ok = file:close(File),
    State2 = State#state{
               files=[{Name, Proplist}|Files],
               file_opts=undefined
              },
    {ok, State2}.

finish(#state{files=Files, props=Props}) ->
    {ok,
     [
      {files, lists:reverse(Files)},
      {props, lists:reverse(Props)}
     ]}.

terminate(_Reason, #state{files=Files, file_opts=FileOpts}) ->
    lists:foreach(
      fun({_, Proplist}) ->
              {_, Path} = lists:keyfind(path, 1, Proplist),
              _ = file:delete(Path)
      end, Files),
    case FileOpts of
        undefined ->
            ok;
        _ ->
            _ = file:close(FileOpts#file_opts.file),
            _ = file:delete(FileOpts#file_opts.path)
    end.

%% ===================================================================
%%% Internal functions
%% ===================================================================

-spec tmp_filename(OriginalName :: binary()) -> string() | [string()].
tmp_filename(_OriginalName) ->
    atom_to_list(?MODULE) ++ integer_to_list(erlang:phash2(make_ref())).
