%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Action queue.
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
-module(oni_aq_proto).

%% API
-export([start_link/1, queue/2, clear/1]).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
    spawn_link(fun() -> loop(queue:new()) end).

queue(Pid, MFA) ->
    Pid ! {self(), {queue, MFA}},
    receive
        Any -> Any
    end.

clear(Pid) -> Pid ! clear.

%%%============================================================================
%%% Internal functions
%%%============================================================================

loop(Queue) ->
    receive
        {From, {queue, MFA}} -> 
            NewQueue = queue:in(MFA, Queue),
            case queue:is_empty(Queue) of
                true -> execute(self(), MFA), From ! ok;
                false -> From ! queued
            end,
            loop(NewQueue);
        next ->
            {_Completed, NewQueue} = queue:out(Queue),
            case queue:peek(NewQueue) of
                {value, MFA} -> execute(self(), MFA), ok;
                empty -> ok
            end,
            loop(NewQueue);
        clear ->
            NewQueue = queue:new(),
            loop(NewQueue);
        _Junk -> 
            loop(Queue)
    end.

execute(Pid, MFA) ->
    F = fun() -> 
        R = rt:execute(MFA),
        case R of
            {continue, Time, CMFA} ->
                timer:sleep(Time),
                execute(Pid, CMFA),
                ok;
            _Other -> Pid ! next, ok
        end
    end,
    spawn(F).