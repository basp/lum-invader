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

-export([list/2, verb/2]).

-type match() :: 
    any() | nothing | {ambiguous, First::binary(), Second::binary()}.

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Tries to match the prefix on a list of binary strings.
-spec list(Prefix::binary(), List::binary()) -> match().
list(Prefix, List) ->
    list(Prefix, List, nothing).

-spec verb(Str::binary(), Verb::binary()) -> boolean().
verb(Str, Verb) ->
    verb(Str, Verb, false).

verb(<<>>, <<>>, _) -> true;
verb(<<X, Str/binary>>, <<X, Verb/binary>>, StarFound) ->
    verb(Str, Verb, StarFound);
verb(<<>>, <<"*", _/binary>>, _) -> true;
verb(_, <<"*">>, _) -> true;
verb(Str, <<"*", Verb/binary>>, _) ->
    verb(Str, Verb, true);
verb(<<>>, _, true) -> true;
verb(_, <<>>, _) -> false;
verb(_, _, _) -> false.

%%%============================================================================
%%% Internal functions
%%%============================================================================

list(<<>>, _List, _Found) -> nothing;
list(_Prefix, [], Found) -> Found;
list(Prefix, [H|T], Found) ->
    case oni_bstr:starts_with(Prefix, H) of
        true when Found =/= nothing -> {ambiguous, Found, H};
        true -> list(Prefix, T, H);
        false -> list(Prefix, T, Found)
    end.
