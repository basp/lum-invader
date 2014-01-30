%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc This is the main supervisor.
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
    SockservSup   = {oni_sockserv_sup, {oni_sockserv_sup, start_link, []},
                    permanent, 2000, supervisor, [oni_sockserv_serv]},
    EventManager  = {oni_event, {oni_event, start_link, []},
                    permanent, 2000, worker, [oni_event]},
    RtServ        = {oni_rt_serv, {oni_rt_serv, start_link, []},
                    permanent, 2000, worker, [oni_rt_serv]},
    AqSup         = {oni_aq_sup, {oni_aq_sup, start_link, []},
                    permanent, 2000, supervisor, [oni_aq_sup]},
    TaskSup       = {oni_task_sup, {oni_task_sup, start_link, []},
                    permanent, 2000, supervisor, [oni_task_sup]},
    ActorSup      = {oni_actor_sup, {oni_actor_sup, start_link, []},
                    permanent, 2000, supervisor, [oni_actor_sup]},
    Children = [SockservSup, EventManager, RtServ, AqSup, TaskSup, ActorSup],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.