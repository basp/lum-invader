%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://github.com/basp]
%%% @copyright 2013-2014
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_sockserv_serv).
-behaviour(gen_server).

-record(state, {socket, next}).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

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
    {ok, #state{socket = Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{socket = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    oni_sockserv_sup:start_socket(),
    notify(AcceptSocket, "Oni [Lum Invader]"),
    {noreply, S#state{next = login}}.

handle_info({tcp, Socket, Data}, S = #state{next = login}) ->
    notify(Socket, Data),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================    
%%% Internal functions
%%%============================================================================
notify(Socket, Data) -> 
    notify(Socket, Data, []).

notify(Socket, Data, _Args) ->
    ok = gen_tcp:send(Socket, [Data, <<$\r, $\n>>]),
    ok = inet:setopts(Socket, [{active, once}, {mode, binary}]),
    ok.