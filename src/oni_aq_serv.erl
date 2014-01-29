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

-record(state, {objid, queue}).

%% API
-export([start_link/1, queue/4, queue/2, next/1, clear/1, queued/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MAX_QUEUE, 25).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Starts an action queue server.
-spec start_link(Obj::oni_db:objid()) -> {ok, pid()}.
start_link(Obj) ->
    {ok, Pid} = gen_server:start_link(?MODULE, Obj, []),
    oni_event:action_queue_started(Obj, Pid),
    %% We should only arrive here via a call to oni_aq_sup which should take
    %% care not too boot up too much `aq` servers. It seems to work fine for 
    %% now but this might be where dragons roam.
    oni_aq:insert(Obj, Pid),
    {ok, Pid}.

%% @doc Queues an action item.
-spec queue(Pid::pid(), Module::atom(), Function::atom(), Args::[any()]) -> 
    queued | ok.
queue(Pid, Module, Function, Args) ->
    gen_server:call(Pid, {queue, {Module, Function, Args}}).

%% @doc Queues an action item using MFA form.
-spec queue(Pid::pid(), {Module::atom(), Function::atom(), Args::[any]}) ->
    queued | ok.
queue(Pid, MFA) ->
    gen_server:call(Pid, {queue, MFA}).

queued(Pid) ->
    gen_server:call(Pid, queued).

%% @doc Clears the queue.
-spec clear(Pid::pid()) -> ok.
clear(Pid) ->
    gen_server:cast(Pid, clear).

%% @doc Signals the queue to continue with the next action item.
%% Note, this method is used internally to signal the completion of
%% long running (continuation) actions.
%% @end
-spec next(Pid::pid()) -> ok.
next(Pid) ->
    gen_server:cast(Pid, next).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

init(Obj) ->
    {ok, #state{objid = Obj, queue = queue:new()}}.

handle_call({queue, MFA}, _From, S = #state{queue = Queue}) ->
    NewQueue = queue:in(MFA, Queue),
    case queue:len(Queue) of
        0 -> 
            %% The initial queue was empty so start executing immediately.
            execute(self(), MFA), 
            {reply, ok, S#state{queue = NewQueue}};
        Len when Len >= ?MAX_QUEUE -> 
            %% Max queue size reached, just drop the action.
            {reply, {full, ?MAX_QUEUE}, S#state{queue = Queue}};
        _ -> 
            %% Queue the action.
            {reply, queued, S#state{queue = NewQueue}}
    end;
handle_call(queued, _From, S = #state{queue = Queue}) ->
    {reply, Queue, S}.

handle_cast(clear, S) ->
    {noreply, S#state{queue = queue:new()}};
handle_cast(next, S = #state{queue = Queue}) ->
    {_, NewQueue} = queue:out(Queue),
    case queue:peek(NewQueue) of
        {value, MFA} -> 
            oni_task_sup:start_task(self(), MFA);
            %% execute(self(), MFA);
        empty -> ok
    end,
    {noreply, S#state{queue = NewQueue}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{objid = Obj}) ->
    %% Doesn't really seem to get here...
    oni_event:action_queue_stopped(Obj, self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================
%%% Internal functions.
%%%============================================================================

execute(Pid, {Module, Function, Args}) ->
    F = fun() -> 
        R = oni_rt_serv:exec(Module, Function, Args),
        case R of
            {continue, Time, CMFA} -> 
                timer:sleep(Time), 
                execute(Pid, CMFA);
            _Other -> 
                next(Pid)
        end
    end,
    spawn(F).