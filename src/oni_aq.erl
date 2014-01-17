%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Operations for the action queue table.
%%%
%%% These are in a separate module because they are used by both the
%%% action queue supervisor and the action queue server modules.
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%% @end
%%%-----------------------------------------------------------------------------
module(oni_aq).

-export([init/0, lookup/1, insert/2]).

-define(TABLE_AQS, oni_action_queues).

-record(queue, {objid = nothing, queues = []}).

init() ->
    ets:new(?TABLE_AQS, [set, {keypos, #queue.objid}, named_table, public]).

lookup(Obj) ->
    case ets:lookup(?TABLE_AQS, Obj) of
        [] -> [];
        [#queue{queues = [Pid]}] -> [Pid]
    end.

insert(Obj, Pid) ->
    ets:insert(?TABLE_AQS, #queue{objid = Obj, queues = [Pid]}).