-module(oni_app).
-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%============================================================================
%%% application callbacks
%%%============================================================================
start(_Args, _Type) ->
    oni_sockserv_sup:start_link().

stop(_State) ->
    ok.