%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_parse).

-export([tokens/1, string/1]).

-type prepstr() :: string() | none.

%%%============================================================================
%%% API 
%%%============================================================================
%% @doc Seperates the given string into tokens.
%% Any tokens between double quotes will be combined into a single one.
-spec tokens(string()) -> [string()].
tokens(Str) -> 
    Tokens = tokens(Str, [], []),
    lists:filter(fun(X) -> X =/= [] end, Tokens).

%% @doc Parses the given string into a command tuple.
-spec string(string()) -> {string(), string(), prepstr(), string()}.
string(Str) ->
    [Cmd|Args] = oni_scan:tokens(Str),
    {Dobj, Prep, Iobj} = string(Args, []),
    {Cmd, string:join(Dobj, " "), Prep, string:join(Iobj, " ")}.

%%%============================================================================
%%% Internal functions
%%%============================================================================
string([T1,T2,T3|Rest], Dobj) ->
    {S2, S3} = {string:join([T1, T2], " "), string:join([T1, T2, T3], " ")},
    case {is_prep(T1), is_prep(S2), is_prep(S3)} of
        {_, _, true} -> {Dobj, S3, Rest};
        {_, true, _} -> {Dobj, S2, Rest ++ [T3]};
        {true, _, _} -> {Dobj, T1, Rest ++ [T2, T3]};
        _ -> string([T2, T3] ++ Rest, Dobj ++ [T1])
    end;
string([T1,T2|Rest], Dobj) ->
    S2 = string:join([T1, T2], " "),
    case {is_prep(T1), is_prep(S2)} of
        {_, true} -> {Dobj, S2, Rest};
        {true, _} -> {Dobj, T1, Rest ++ [T2]};
        _ -> string([T2] ++ Rest, Dobj ++ [T1])
    end;
string([T1|Rest], Dobj) ->
    case is_prep(T1) of
        true -> {Dobj, T1, Rest};
        _ -> string(Rest, Dobj ++ [T1])
    end;
string([], Dobj) -> {Dobj, none, []}.

is_prep(Str) ->
    case Str of
        "with" -> true;
        "at" -> true;
        "to" -> true;
        "in front of" -> true;
        "in" -> true;
        "from" -> true;
        "inside" -> true;
        "into" -> true;
        "on top of" -> true;
        "on" -> true;
        "onto" -> true;
        "upon" -> true;
        "using" -> true;
        "out of" -> true;
        "from inside" -> true;
        "over" -> true;
        "through" -> true;
        "under" -> true;
        "underneath" -> true;
        "behind" -> true;
        "for" -> true;
        "about" -> true;
        "is" -> true;
        "as" -> true;
        "off" -> true;
        "off of" -> true;
        _ -> false
    end.

%% @author Hyperboreus [http://stackoverflow.com/users/763505/hyperboreus]
%% http://stackoverflow.com/questions/6901598/tokenize-quoted-string
tokens([], Tokens, Buffer) ->
    F = fun(X) -> string:strip(X, both, $") end,
    lists:map(F, Tokens ++ [Buffer]);
tokens([Character | String], Tokens, Buffer) ->
    case {Character, Buffer} of
        {$ , [] } -> tokens(String, Tokens, Buffer);
        {$ , [$" | _]} -> tokens (String, Tokens, Buffer ++ [Character]);
        {$ , _} -> tokens(String, Tokens ++ [Buffer], []);
        {$", [] } -> tokens(String, Tokens, "\"");
        {$", [$" | _]} -> tokens (String, Tokens ++ [Buffer ++ "\""], []);
        {$", _} -> tokens(String, Tokens ++ [Buffer], "\"");
        _ -> tokens(String, Tokens, Buffer ++ [Character])
    end.