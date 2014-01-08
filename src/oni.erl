%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).

-export([do_login/2, start/0, stop/0, notify/2, notify/3]).

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
notify(Socket, Str) ->
    notify(Socket, Str, []).

%% @doc Send a format message to specified socket.
notify(Socket, Str, Args) -> 
    ok = gen_tcp:send(Socket, [io_lib:format(Str, Args), <<$\r, $\n>>]),
    ok = inet:setopts(Socket, [{active, once}, {mode, binary}]).

find_players(Name) ->
    lists:filter(fun(Id) -> oni_db:name(Id) =:= Name end, oni_db:players()).