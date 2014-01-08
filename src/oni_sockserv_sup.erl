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
-module(oni_sockserv_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_socket/0]).

%% supervisor callbacks
-export([init/1]).

%%%============================================================================    
%%% API
%%%============================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_socket() ->
    supervisor:start_child(?MODULE, []).

%%%============================================================================    
%%% supervisor callbacks
%%%============================================================================

init([]) ->
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, once}]),
    spawn_link(fun ready_listeners/0),
    Strategy = {simple_one_for_one, 60, 3600},
    Spec = {socket, {oni_sockserv_serv, start_link, [ListenSocket]},
            temporary, 1000, worker, [oni_sockserv_serv]},
    {ok, {Strategy, [Spec]}}.

%%%============================================================================    
%%% Internal functions
%%%============================================================================

ready_listeners() ->
    [start_socket() || _ <- lists:seq(1, 20)],
    ok.