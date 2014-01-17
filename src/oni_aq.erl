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

-export([init/0, start/1, queue/2, queue/4, clear/1]).

-define(TABLE_AQS, oni_action_queues).

-record(queue, {objid = nothing, queues = []}).

start(Obj) ->
    NewValue = 
        case ets:lookup(?TABLE_AQS, Obj) of
            [] -> 
                {ok, Pid} = oni_aq_sup:start_queue(Obj),
                oni_event:action_queue_started(Obj),
                [#queue{objid = Obj, queues = [Pid]}];
            [#queue{queues = [Pid]}] ->
                [#queue{objid = Obj, queues = [Pid]}]
        end,
    Pid.

clear(Obj) ->
    case ets:lookup(?TABLE_AQS, Obj) of
        [] -> false;
        [#queue{queues = [Pid]}] -> oni_aq_serv:clear(Pid)
    end.

queue(Obj, Pack) ->
    {M, F} = oni_pack:code(Pack),
    Bindings = oni_pack:bindings(Pack),
    Player = proplists:get_value(player, Bindings),
    Verb = proplists:get_value(verb, Bindings),
    case queue(Obj, M, F, [Bindings]) of
        queued -> oni:notify(Player, "[ queued - '~s' ]", [Verb]);
        full -> oni:notify(Player, "[ max queue size reached ]", []);
        _ -> ok       
    end.

queue(Obj, M, F, A) ->
    case ets:lookup(?TABLE_AQS, Obj) of
        [] -> 'E_INVARG';
        [#queue{queues = [Pid|_]}] ->
            oni_aq_serv:queue(Pid, M, F, A)
    end.