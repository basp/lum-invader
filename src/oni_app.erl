%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_app).
-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%============================================================================
%%% application callbacks
%%%============================================================================
start(_Args, _Type) ->
	oni_db:init(),
    oni_sup:start_link().

stop(_State) ->
    ok.