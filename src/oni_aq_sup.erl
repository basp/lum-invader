%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Implements the callbacks for the accept socket supervisor.
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
-export([start_link/0, start_queue/1]).

%% supervisor callbacks
-export([init/1]).

%%%============================================================================    
%%% API
%%%============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_queue(Obj) ->
    supervisor:start_child(?MODULE, [Obj]).

%%%============================================================================    
%%% supervisor callbacks
%%%============================================================================

init(_Obj) ->
    Strategy = {simple_one_for_one, 60, 3600},
    Spec = {aq, {oni_aq_serv, start_link, []},
            permanent, 1000, worker, [oni_aq_serv]},
    {ok, {Strategy, [Spec]}}.

%%%============================================================================    
%%% Internal functions
%%%============================================================================