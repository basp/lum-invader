%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
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
    error_logger:info_msg("~p ~p connected~n", [Socket, Address]),
    {ok, State};
handle_event({disconnected, Socket}, State) ->
    error_logger:info_msg("~p disconnected~n", [Socket]),
    {ok, State};
handle_event({player_connected, Socket, Player}, State) ->
    error_logger:info_msg("Player ~p connected on ~p~n", [Player, Socket]),
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