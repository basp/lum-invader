%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Event stream API.
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
-module(oni_event).

-export([start_link/0, 
         add_handler/2, delete_handler/2, 
         connected/1, disconnected/1, 
         player_connected/2, player_disconnected/2,
         action_queue_started/2, action_queue_stopped/2,
         runtime_pack_exec/1, runtime_mfa_exec/1]).

-define(SERVER, ?MODULE).

%% @doc Starts the event server.
start_link() ->
    gen_event:start_link({local, ?SERVER}).

%% @doc Adds an event handler.
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%% @doc Deletes an event handler.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

%% @doc Sends the connected event for anonymous connections.
connected(Socket) ->
    gen_event:notify(?SERVER, {connected, Socket}).

%% @doc Sends the disconnected event.
disconnected(Socket) ->
    gen_event:notify(?SERVER, {disconnected, Socket}).

%% @doc Sends the player_connected event.
player_connected(Socket, Player) ->
	gen_event:notify(?SERVER, {player_connected, Socket, Player}).

player_disconnected(Socket, Player) ->
    gen_event:notify(?SERVER, {player_disconnected, Socket, Player}).

action_queue_started(Obj, Pid) ->
    gen_event:notify(?SERVER, {action_queue_started, Obj, Pid}).

action_queue_stopped(Obj, Pid) ->
    gen_event:notify(?SERVER, {action_queue_stopped, Obj, Pid}).

runtime_pack_exec(Pack) ->
    gen_event:notify(?SERVER, {runtime_pack_exec, Pack}).

runtime_mfa_exec(MFA) ->
    gen_event:notify(?SERVER, {runtime_mfa_exec, MFA}).