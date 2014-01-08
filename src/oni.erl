%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).

-compile(export_all).

%% Default login handler
do_login(Socket, {<<"connect">>, _Dobjstr, Argstr, _Args}) ->
    case find_players(Argstr) of
        [] -> notify(Socket, "Invalid player or password."), nothing;
        [Id] -> notify(Socket, "*** connected (~s) ***", [Argstr]), Id
    end;
do_login(Socket, Command) ->
    notify(Socket, "~p", [Command]),
    nothing.

find_players(Name) ->
    lists:filter(fun(Id) -> oni_db:name(Id) =:= Name end, oni_db:players()).

%% Application control
start() ->
    application:start(oni).

stop() ->
    application:stop(oni).

%% Core output functions
notify(Socket, Str) ->
    notify(Socket, Str, []).

notify(Socket, Str, Args) -> 
    ok = gen_tcp:send(Socket, [io_lib:format(Str, Args), <<$\r, $\n>>]),
    ok = inet:setopts(Socket, [{active, once}, {mode, binary}]).