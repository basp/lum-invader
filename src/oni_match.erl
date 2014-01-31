%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Matching functions.
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
-module(oni_match).

-export([list/2, list_i/3, verb/2]).

-export_type([match/0]).

-type match() :: any() | failed | {ambiguous, [binary()]}.

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Matches the predicate on a list of binary strings.
%% 
%% This might return {ambiguous, [binary()]} when there is more than one match.
-spec list(Pred::fun((binary()) -> boolean()), List::[binary()]) -> match().
list(Pred, List) ->
    list(Pred, List, failed).

%% @doc Like list but returns the nth match or nothing instead.
%%
%% In contrast with list, this will never return ambiguous.
-spec list_i(Pred::fun((binary()) -> boolean()), 
             List::[binary()], Index::integer()) -> nothing | binary().
list_i(Pred, List, Index) when Index > 0 -> 
    list_i(Pred, List, [], Index).

%% @doc Matches verb names including the wildcard asterisk.
-spec verb(Str::binary(), Verb::binary()) -> boolean().
verb(Str, Verb) ->
    verb(Str, Verb, false).

%%%============================================================================
%%% Internal functions
%%%============================================================================

list(_Pred, [], Found) -> Found;
list(Pred, [H|T], Found) ->
    case Pred(H) of
        true when Found =/= failed -> {ambiguous, [Found, H]};
        true -> list(Pred, T, H);
        false -> list(Pred, T, Found)
    end.

list_i(_Pred, [], Acc, Index) when length(Acc) < Index, Index > 0 -> 
    nothing;
list_i(_Pred, [], Acc, Index) when Index > 0 ->
    lists:nth(Index, lists:reverse(Acc));
list_i(Pred, [H|T], Acc, Index) when Index > 0 ->
    case Pred(H) of
        true -> list_i(Pred, T, [H|Acc], Index);
        false -> list_i(Pred, T, Acc, Index)
    end.

%% Verb function below
%%
%% Two empty binaries are equal. This is required for our end condition.
verb(<<>>, <<>>, _) -> true;

%% The X byte is the same on both left and right so we are still equal.
verb(<<X, Str/binary>>, <<X, Verb/binary>>, StarFound) ->
    verb(Str, Verb, StarFound);

%% We ran out of left side bytes but found a wildcard and some other stuff.
%% At this point our left part matched completely so we can return true.
verb(<<>>, <<"*", _/binary>>, _) -> true;

%% We ran into the end of our match expression and it ended with a wildcard.
%% Whatever there is on the left side we can ignore - we have matched.
verb(_, <<"*">>, _) -> true;

%% We are running into a wildcard but there is more to parse. We also have
%% more to parse on the left side so we'll try and see if we can continue.
%%
%% At this point the thing specified by the user is more precise (longer) than 
%% is required by the thing that has the wildcard. Because the user supplied 
%% expression is longer we are going to continue and match more precisely.
verb(Str, <<"*", Verb/binary>>, _) ->
    verb(Str, Verb, true);

%% We ran out on the left side, but found an asterisk earlier. If we arrived 
%% this far, that means our left part completed matched everything on the right
%% side.
verb(<<>>, _, true) -> true;

%% At this point we ran out of characters to match with on the right but there 
%% are more characters on the left. The left is more specific so it doesn't
%% really match.
verb(_, <<>>, _) -> false;

%% If we arrive here we are totally lost. We might wanna add some debugging to
%% this code point later because getting here is not good.
verb(_, _, _) -> false.
