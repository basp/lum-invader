%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Action queue server supervisor.
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
-module(oni_aq_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_queue/1, queue/2, clear/1]).

%% supervisor callbacks
-export([init/1]).

%%%============================================================================    
%%% API
%%%============================================================================

start_link() ->
    oni_aq:init(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_queue(Obj) ->
    case oni_aq:lookup(Obj) of
        [] -> supervisor:start_child(?MODULE, [Obj]);
        [Pid] -> {already_running, Pid}
    end.

clear(Obj) ->
    case oni_aq:lookup(Obj) of
        [] -> false;
        [Pid] -> oni_aq_serv:clear(Pid)
    end.

queue(Obj, Pack) ->
    {M, F} = oni_pack:code(Pack),
    Bindings = oni_pack:bindings(Pack),
    Player = proplists:get_value(player, Bindings),
    Verb = proplists:get_value(verb, Bindings),
    case queue(Obj, M, F, [Bindings]) of
        queued -> oni:notify(Player, "[ queued - '~s' ]", [Verb]);
        {full, Max} -> oni:notify(Player, "[ cannot queue -- ~p actions already ]", [Max]);
        _ -> ok       
    end.

%%%============================================================================    
%%% supervisor callbacks
%%%============================================================================

init([]) ->
    Strategy = {simple_one_for_one, 60, 3600},
    Spec = {aq, {oni_aq_serv, start_link, []},
            permanent, 1000, worker, [oni_aq_serv]},
    {ok, {Strategy, [Spec]}}.

%%%============================================================================    
%%% Internal functions
%%%============================================================================

queue(Obj, M, F, A) ->
    case oni_aq:lookup(Obj) of
        [] -> 'E_INVARG';
        [Pid] -> oni_aq_serv:queue(Pid, M, F, A)
    end.
