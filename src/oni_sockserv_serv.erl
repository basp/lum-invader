%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_sockserv_serv).
-behaviour(gen_server).

-record(state, {listener, 
                next, 
                login_handler = {oni, defaut_login_handler, []}}).

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
    {ok, #state{listener = Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{listener = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    oni_sockserv_sup:start_socket(),
    oni_event:connected(AcceptSocket),
    oni:notify(AcceptSocket, "Oni [Lum Invader]"),
    {noreply, S#state{next = login}}.
    
handle_info({tcp, Socket, Data}, S = #state{next = login}) ->
    Command = oni_cmd:parse(Data),
    case oni:do_login(Socket, Command) of
        nothing -> {noreply, S};
        _ -> {noreply, S}
    end;
handle_info({tcp_closed, Socket}, State) ->
    oni_event:disconnected(Socket),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.