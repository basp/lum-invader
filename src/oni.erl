%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Main client API. 
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
-module(oni).

-export([test/0, 
         do_login/3, 
         start/0, stop/0, 
         notify/2, notify/3, 
         say/2, emote/2,
         eval/2, eval_to_str/2]).

-define(INVALID_LOGIN, <<"Invalid player or password">>).

%% @doc Development utilitities
test() ->
    eunit:test([oni_ansi, oni_bstr, oni_cmd, oni_match]).

%% @doc Default login handler
do_login(Socket, <<"connect">>, [Name|_]) ->
    case find_players(Name) of
        [] -> notify(Socket, ?INVALID_LOGIN), nothing;
        [Id] -> notify(Socket, "*** connected (~s) ***", [Name]), Id
    end;
do_login(Socket, _Command, _Args) ->
    notify(Socket, ?INVALID_LOGIN),
    nothing.

%% @doc Starts the application.
start() ->
    application:start(oni).

%% @doc Stops the application.
stop() ->
    application:stop(oni).

say(User, Data) ->
    Say = case binary:last(Data) of
        $!  -> <<"exlaims">>;
        $?  -> <<"asks">>;
        _   -> <<"says">>
    end,
    Name = oni_db:name(User),
    Msg = <<Name/binary, " ", Say/binary, ", \"", Data/binary, "\"">>,
    Contents = case oni_db:location(User) of
        nothing -> [User];
        Location -> oni_db:contents(Location)
    end,
    Players = lists:filter(fun(X) -> oni_db:is_player(X) end, Contents),
    lists:foreach(fun(X) -> notify(X, Msg) end, Players).

emote(User, Data) ->
    Name = oni_db:name(User),
    Msg = <<Name/binary, " ", Data/binary>>,
    Contents = case oni_db:location(User) of
        nothing -> [User];
        Location -> oni_db:contents(Location)
    end,
    Players = lists:filter(fun(X) -> oni_db:is_player(X) end, Contents),
    lists:foreach(fun(X) -> notify(X, Msg) end, Players).

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