%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Runtime server (sync verb execution).
%%%
%%% Used internally for the execution queue. This is the main game loop,
%%% everything that is executed as a result of user input goes through
%%% this server.
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
-module(oni_rt_serv).
-behaviour(gen_server).

-record(state, {}).

%% API
-export([start_link/0, exec/1, exec/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%============================================================================    
%%% API 
%%%============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

exec(Pack) ->
    gen_server:call(?SERVER, {exec, Pack}).

exec(M, F, A) ->
    gen_server:call(?SERVER, {exec, M, F, A}).

%%%============================================================================    
%%% gen_server callbacks
%%%============================================================================

init([]) ->
    {ok, #state{}}.

handle_call({exec, Pack}, _From, State) ->
    oni_event:runtime_pack_exec(Pack),
    Bindings = oni_pack:bindings(Pack),
    {M, F} = oni_pack:code(Pack),
    R = M:F(Bindings),
    {reply, R, State};
handle_call({exec, M, F, A}, _From, State) ->
    oni_event:runtime_mfa_exec({M, F, A}),
    R = apply(M, F, A),
    {reply, R, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.