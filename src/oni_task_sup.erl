-module(oni_task_sup).

-export([start_link/0, start_task/2, start_task/4, spawn_task/2]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_task(Pid, MFA) ->
    supervisor:start_child(?MODULE, [Pid, MFA]).

start_task(Pid, M, F, A) ->
    supervisor:start_child(?MODULE, [Pid, {M, F, A}]).

init([]) ->
    Strategy = {simple_one_for_one, 60, 3600},
    Spec = {task, {?MODULE, spawn_task, []},
                  temporary, 1000, worker, [oni_task_sup]},
    {ok, {Strategy, [Spec]}}.

spawn_task(Pid, MFA) ->
    F = fun() -> execute(Pid, MFA) end,
    {ok, spawn_link(F)}.

execute(Pid, {Module, Function, Args}) ->
    Result = oni_rt_serv:exec(Module, Function, Args),
    case Result of
        {continue, Time, CMFA} -> 
            %% Sleep for a while...
            timer:sleep(Time), 
            %% And then continue with the rest of our action
            execute(Pid, CMFA);
        _Other -> 
            %% Signal next to our parent queue process
            oni_aq_serv:next(Pid)
    end.

