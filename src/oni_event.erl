%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Event stream API.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_event).

-export([start_link/0, add_handler/2, delete_handler/2, 
         connected/1, disconnected/1]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

connected(Socket) ->
    gen_event:notify(?SERVER, {connected, Socket}).

disconnected(Socket) ->
    gen_event:notify(?SERVER, {disconnected, Socket}).

%% player_connected, player_disconnected, etc.