%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_match).

-export([string/2]).

-type match() :: any() | nothing | ambiguous.

%%%============================================================================
%%% API
%%%============================================================================
%% @doc Tries to match given string with a list of strings.
-spec string(string(), [string()]) -> match().
string(Needle, Stack) -> string(Needle, Stack, nothing).

%%%============================================================================
%%% Internal functions
%%%============================================================================
string([], _Stack, _Found) -> nothing;
string(_Needle, [], Found) -> Found;
string(Needle, [H|T], Found) ->
    Pattern = io_lib:format("^~s", [Needle]),   
    case re:run(H, Pattern, [{capture, none}]) of
        match when Found =/= nothing -> ambiguous;
        match -> string(Needle, T, H);
        nomatch -> string(Needle, T, Found)
    end.    