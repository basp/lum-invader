%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Actor supervisor.
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
-module(oni_actor_sup).

%% API
-export([start_link/0, start_actor/2]).

%% supervisor callbacks
-export([init/1]).

%% Internal
-export([spawn_actor/2]).

%%%============================================================================
%%% API
%%%============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_actor(Obj, {Module, Function}) ->
    supervisor:start_child(?MODULE, [Obj, {Module, Function}]).

%%%============================================================================
%%% supervisor callbacks
%%%============================================================================

init([]) ->
    Strategy = {simple_one_for_one, 60, 3600},
    Spec = {task, {?MODULE, spawn_actor, []},
                  temporary, 1000, worker, [oni_actor_sup]},
    {ok, {Strategy, [Spec]}}.

%%%============================================================================
%%% Internal functions.
%%%============================================================================

spawn_actor(Obj, {Module, Function}) -> 
    oni_aq_sup:start_queue(Obj),
    {ok, spawn_link(Module, Function, [Obj])}.
