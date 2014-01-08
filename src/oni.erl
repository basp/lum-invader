%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).

-export([do_login/2, start/0, stop/0, notify/2, notify/3, 
         eval/2, eval_to_str/2]).

%% @doc Default login handler
do_login(Socket, {<<"connect">>, _Dobjstr, Argstr, _Args}) ->
    case find_players(Argstr) of
        [] -> notify(Socket, "Invalid player or password."), nothing;
        [Id] -> notify(Socket, "*** connected (~s) ***", [Argstr]), Id
    end;
do_login(Socket, Command) ->
    notify(Socket, "~p", [Command]),
    nothing.

%% @doc Starts the application.
start() ->
    application:start(oni).

%% @doc Stops the application.
stop() ->
    application:stop(oni).

%% @doc Send a message to specified socket.
notify(Target, Str) ->
    notify(Target, Str, []).

%% @doc Send a format message to specified socket.
notify(Target, Str, Args) when is_integer(Target) -> 
    case oni_who:get_connection(Target) of
        nothing -> ok;
        Socket -> notify(Socket, Str, Args)
    end;
notify(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, [io_lib:format(Str, Args), <<$\r, $\n>>]),
    ok = inet:setopts(Socket, [{active, once}, {mode, binary}]).

%% @doc Evaluates Erlang code to a string.
eval_to_str(Str, Bindings) ->
    try eval(Str, Bindings) of 
        {[Val], NewBindings} -> 
            {io_lib:format("=> ~p", [Val]), NewBindings};
        {List, NewBindings} -> 
            {io_lib:format("=> ~p", [List]), NewBindings}
    catch
        Exception:Reason -> 
            {io_lib:format("~p: ~p", [Exception, Reason]), Bindings}
    end.

%% @doc Evaluates Erlang code.
eval(Str, Bindings) when is_binary(Str) ->
    eval(binary_to_list(Str), Bindings);
eval(Str, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, ExprList} = erl_parse:parse_exprs(Tokens),
    erl_eval:expr_list(ExprList, Bindings).

find_players(Name) ->
    lists:filter(fun(Id) -> oni_db:name(Id) =:= Name end, oni_db:players()).