-module(oni).

%% API
-export([
    match/2, 
    starts_with/2, ends_with/2, 
    between/2, 
    tokens/1, 
    delimited/2,
    parse/1,
    first/2]).

%%%============================================================================
%%% API
%%%============================================================================
match(Needle, Stack) -> 
    match(Needle, Stack, nothing).

%% http://stackoverflow.com/a/12657896
first(Pred, List) ->
    case lists:dropwhile(fun(X) -> not Pred(X) end, List) of
        [] -> nothing;
        [X | _] -> X
    end.

parse(Str) ->
    [Cmd|Args] = tokens(Str),
    {Cmd, Args, string:join(Args, " ")}.

tokens(Str) ->
    RawTokens = string:tokens(Str, " \t\r\n"),
    tokens(RawTokens, []).

delimited(Delimiters, Tokens) -> 
    delimited(Delimiters, Tokens, []).

between(_, []) -> {false, false};
between(Needles, Stack) ->
    StartsWith = starts_with(Needles, Stack),
    EndsWith = ends_with(Needles, Stack),
    {StartsWith, EndsWith}.

starts_with(_, []) -> false;
starts_with(Needles, [First|_]) ->
    lists:any(fun(X) -> X =:= First end, Needles).

ends_with(_, []) -> false;
ends_with(Needles, Stack) ->
    Last = lists:last(Stack),
    lists:any(fun(X) -> X =:= Last end, Needles).

%%%============================================================================
%%% Internal functions
%%%============================================================================
tokens([], Tokens) -> lists:reverse(Tokens);
tokens([H|T], Tokens) ->
    case starts_with("\"'", H) of
        true -> 
            [_|Tok] = H,
            {Rest, Delimited} = delimited("\"'", [Tok|T]),
            tokens(Rest, [string:join(Delimited, " ")|Tokens]);
        false ->
            tokens(T, [H|Tokens])
    end.

delimited(_Delimiters, [], S) -> {[], lists:reverse(S)};
delimited(Delimiters, [H|T], S) ->
    case ends_with(Delimiters, H) of
        true -> 
            {T, lists:reverse([string:sub_string(H, 1, string:len(H) -1)|S])};
        false -> delimited(Delimiters, T, [H|S])
    end.

match([], _Stack, _Found) -> nothing;
match(_Needle, [], Found) -> Found;
match(Needle, [Thing|Rest], Found) ->
    Pattern = io_lib:format("^~s", [Needle]),   
    case re:run(Thing, Pattern, [{capture, none}]) of
        match when Found =/= nothing -> ambiguous;
        match -> match(Needle, Rest, Thing);
        nomatch -> match(Needle, Rest, Found)
    end.    