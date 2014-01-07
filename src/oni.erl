%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni).

-compile(export_all).

%% Default login handler
do_login(Socket, Command = {<<"connect">>, _Dobjstr, Argstr, _Args}) ->
    notify(Socket, "Login with ~p", [Argstr]),
    %% TODO: Get player from db, add return player id
    notify(Socket, "~p", [Command]),
    nothing;
do_login(Socket, Command) ->
    notify(Socket, "~p", [Command]),
    nothing.

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
    %% We'll just set opts after each notify, we explicitly set mode 
    %% to binary because we like to parse the raw bytes for speed.
    %% This also means we don't support unicode for input.
    ok = inet:setopts(Socket, [{active, once}, {mode, binary}]).