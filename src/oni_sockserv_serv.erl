%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_sockserv_serv).
-behaviour(gen_server).

-record(state, {listener, 
                next, 
                player = nothing}).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(MSG_CONNECT, <<"$bold;$fg_magenta;Oni $fg_yellow;[$fg_cyan;lum invader$fg_yellow;]$reset;">>).

%%%============================================================================    
%%% API 
%%%============================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%============================================================================    
%%% gen_server callbacks
%%%============================================================================

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{listener = Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{listener = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    oni_sockserv_sup:start_socket(),
    oni_event:connected(AcceptSocket),
    oni:notify(AcceptSocket, oni_ansi:style(?MSG_CONNECT)),
    {noreply, S#state{next = login}}.
    
handle_info({tcp, Socket, Data}, S = #state{next = login}) ->
    Command = oni_cmd:parse(Data),
    case oni:do_login(Socket, Command) of
        nothing -> {noreply, S};
        Player -> 
            oni_event:player_connected(Socket, Player),
            {noreply, S#state{next = connected, player = Player}}
    end;
handle_info({tcp, Socket, Data}, 
             S = #state{next = connected, player = _Player}) ->
    Command = oni_cmd:parse(Data),
    oni:notify(Socket, "~p", [Command]),
    {noreply, S};
handle_info({tcp_closed, Socket}, State) ->
    oni_event:disconnected(Socket),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.