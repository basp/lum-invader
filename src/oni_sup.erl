%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SockservSup = {oni_sockserv_sup, {oni_sockserv_sup, start_link, []},
                   permanent, 2000, supervisor, [oni_sockserv_serv]},
    EventManager = {oni_event, {oni_event, start_link, []},
                    permanent, 2000, worker, [oni_event]},
    Children = [SockservSup, EventManager],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.