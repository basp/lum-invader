%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Simple event logger, basically an example.
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
-module(oni_event_logger).

-behaviour(gen_event).

%% API
-export([add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%%============================================================================
%%% API
%%%============================================================================

add_handler() ->
    oni_event:add_handler(?MODULE, []).

delete_handler() ->
    oni_event:delete_handler(?MODULE, []).

%%%============================================================================
%%% gen_event callbacks
%%%============================================================================

init([]) ->
    {ok, #state{}}.

handle_event({connected, Socket}, State) ->
    {ok, {Address, _Port}} = inet:peername(Socket),
    error_logger:info_msg("Anonymous ~p connected on ~p~n", [Address, Socket]),
    {ok, State};
handle_event({disconnected, Socket}, State) ->
    error_logger:info_msg("Anonymous disconnected from ~p~n", [Socket]),
    {ok, State};
handle_event({player_connected, Socket, Player}, State) ->
    Name = oni_db:name(Player),
    error_logger:info_msg("Player #~p (~p) connected on ~p~n", [Player, Name, Socket]),
    {ok, State};
handle_event({player_disconnected, Socket, Player}, State) ->
    Name = oni_db:name(Player),
    error_logger:info_msg("Player #~p (~p) disconnected from ~p~n", [Player, Name, Socket]),
    {ok, State};
handle_event({action_queue_started, Obj, Pid}, State) ->
    Name = oni_db:name(Obj),
    error_logger:info_msg("Started action queue ~p for object #~p (~p)~n", [Pid, Obj, Name]),
    {ok, State};
handle_event({action_queue_stopped, Obj, Pid}, State) ->
    Name = oni_db:name(Obj),
    error_logger:info_msg("Stopped action queue ~p for object #~p (~p)~n", [Pid, Obj, Name]),
    {ok, State};
handle_event({runtime_pack_exec, Pack}, State) ->
    error_logger:info_msg("Runtime pack execution: ~p~n", [Pack]),
    {ok, State};
handle_event({runtime_mfa_exec, MFA}, State) ->
    error_logger:info_msg("Runtime MFA execution: ~p~n", [MFA]),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.