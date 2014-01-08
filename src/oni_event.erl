%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Event stream API.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_event).

-export([start_link/0, add_handler/2, delete_handler/2, 
         connected/1, disconnected/1, player_connected/2]).

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