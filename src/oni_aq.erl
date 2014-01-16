%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Main action queue API.
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
%%%----------------------------------------------------------------------------
-module(oni_aq).

-export([init/0, start_queue/1, queue/4]).

-define(TABLE_AQS, oni_action_queues).

-record(queue, {objid = nothing, queues = []}).

init() ->
    ets:new(?TABLE_AQS, [set, {keypos, #queue.objid}, named_table, public]).

start_queue(Obj) ->
    NewValue = 
        case ets:lookup(?TABLE_AQS, Obj) of
            [] -> 
                {ok, Pid} = oni_aq_sup:start_queue(Obj),
                [#queue{objid = Obj, queues = [Pid]}];
            [#queue{queues = [Pid]}] ->
                [#queue{objid = Obj, queues = [Pid]}]
        end,
    ets:insert(?TABLE_AQS, NewValue),
    Pid.

queue(Obj, M, F, A) ->
    case ets:lookup(?TABLE_AQS, Obj) of
        [] -> 'E_INVARG';
        [#queue{queues = [Pid|_]}] ->
            oni_aq_serv:queue(Pid, M, F, A)
    end.