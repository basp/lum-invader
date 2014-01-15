%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Action queue server.
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
-module(oni_aq_serv).
-behaviour(gen_server).

-record(state, {obj = nothing, queue = queue:new()}).

%% API
-export([start_link/1, queue/4, queue/2, next/1, clear/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%============================================================================
%%% API
%%%============================================================================

-spec start_link(Obj::oni_db:objid()) -> any().
start_link(Obj) ->
    gen_server:start_link(?MODULE, Obj, []).

-spec queue(Pid::pid(), Module::atom(), Function::atom(), Args::[any()]) -> 
    queued | ok.
queue(Pid, Module, Function, Args) ->
    gen_server:call(Pid, {queue, {Module, Function, Args}}).

-spec queue(Pid::pid(), {Module::atom(), Function::atom(), Args::[any]}) ->
    queued | ok.
queue(Pid, MFA) ->
    gen_server:call(Pid, {queue, MFA}).

-spec clear(Pid::pid()) -> ok.
clear(Pid) ->
    gen_server:cast(Pid, clear).

-spec next(Pid::pid()) -> ok.
next(Pid) ->
    gen_server:cast(Pid, next).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

init(Obj) ->
    {ok, #state{obj = Obj}}.

handle_call({queue, MFA}, _From, S = #state{queue = Queue}) ->
    NewQueue = queue:in(MFA, Queue),
    case queue:is_empty(Queue) of
        true -> 
            execute(self(), MFA),
            {reply, ok, S#state{queue = NewQueue}};
        false -> 
            {reply, queued, S}
    end.

handle_cast(clear, S) ->
    {noreply, S#state{queue = queue:new()}};
handle_cast(next, S = #state{queue = Queue}) ->
    {_, NewQueue} = queue:out(Queue),
    case queue:peek(NewQueue) of
        {value, MFA} -> execute(self(), MFA);
        empty -> ok
    end,
    {noreply, S#state{queue = NewQueue}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions.
%%%============================================================================

execute(Pid, {Module, Function, Args}) ->
    F = fun() -> 
        R = apply(Module, Function, Args),
        case R of
            {continue, Time, CMFA} -> timer:sleep(Time), execute(Pid, CMFA);
            _Other -> next(Pid)
        end
    end,
    spawn(F).