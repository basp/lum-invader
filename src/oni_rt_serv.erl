-module(oni_rt_serv).
-behaviour(gen_server).

-record(state, {}).

%% API
-export([start_link/0, exec/1]).

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

%%%============================================================================    
%%% gen_server callbacks
%%%============================================================================

init([]) ->
    {ok, #state{}}.

handle_call({exec, Pack}, _From, State) ->
    Bindings = oni_pack:bindings(Pack),
    {M, F} = oni_pack:code(Pack),
    R = M:F(Bindings),
    {reply, R, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) -> 
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.