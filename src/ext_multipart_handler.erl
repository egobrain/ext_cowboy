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

-module(ext_multipart_handler).

-callback init(Opts) ->
    State
        when Opts :: any(),
            State :: any().

-callback start_of_part(Headers, State) ->
    {ok, State} | {error, Reason}
        when Headers :: [{binary(), binary()}],
             Reason :: any().

-callback part_data(Data, State) ->
    {ok, State} | {error, Reason}
        when Data :: bniary,
             Reason :: any().

-callback end_of_part(State) ->
    {ok, State} | {error, Reason}
        when Reason :: any().

-callback finish(State) ->
    {ok, [{atom(), any()}]} | {error, Reason}
        when State :: any(),
             Reason :: any().

-callback terminate(Reason, State) ->
    ok
        when Reason :: any(),
             State :: any().
