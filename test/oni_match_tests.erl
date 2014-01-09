%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
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
-module(oni_match_tests).

-include_lib("eunit/include/eunit.hrl").

starts_with(X) -> fun(Y) -> oni_bstr:starts_with(X, Y) end.

match_list_test() ->
    ?assertEqual(failed, 
        oni_match:list(starts_with(<<"foo">>), [])),
    ?assertEqual(failed, 
        oni_match:list(starts_with(<<"foo">>), [<<"bar">>])),
    ?assertEqual(<<"foo">>, 
        oni_match:list(starts_with(<<"foo">>), [<<"foo">>, <<"bar">>])),
    ?assertEqual(<<"bar">>, 
        oni_match:list(starts_with(<<"bar">>), [<<"foo">>, <<"bar">>])),
    ?assertEqual(<<"foo">>, 
        oni_match:list(starts_with(<<"fo">>), [<<"bar">>, <<"foo">>])),
    ?assertEqual({ambiguous, [<<"foobar">>, <<"foo">>]}, 
        oni_match:list(starts_with(<<"foo">>), [<<"foobar">>, <<"foo">>])),
    ?assertEqual({ambiguous, [<<"f">>, <<"f">>]}, 
        oni_match:list(starts_with(<<"f">>), [<<"f">>, <<"f">>])).

match_verb(X) -> fun(Y) -> oni_match:verb(X, Y) end.

match_verb_list_test() ->
    ?assertEqual(<<"foo*bar">>, 
        oni_match:list(match_verb(<<"foo">>), [<<"bar*foo">>, <<"foo*bar">>])),
    ?assertEqual(<<"foo*">>,
        oni_match:list(match_verb(<<"foogleman">>), [<<"foo*bar">>, <<"foo*">>])),
    ?assertEqual(<<"*">>,
        oni_match:list(match_verb(<<"frob">>), [<<"foo*bar">>, <<"*">>])),
    ?assertEqual({ambiguous, [<<"foo*bar">>, <<"foo">>]},
        oni_match:list(match_verb(<<"foo">>), [<<"foo*bar">>, <<"foo">>])).

match_verb_test() ->
    %% Containing star matches any prefix of itself that is as least 
    %% as long as the part before the star.
    ?assertEqual(true,  oni_match:verb(<<"foo">>, <<"foo*bar">>)),
    ?assertEqual(true,  oni_match:verb(<<"foob">>, <<"foo*bar">>)),
    ?assertEqual(true,  oni_match:verb(<<"fooba">>, <<"foo*bar">>)),
    ?assertEqual(true,  oni_match:verb(<<"foobar">>, <<"foo*bar">>)),
    ?assertEqual(false, oni_match:verb(<<"fo">>, <<"foo*bar">>)),
    %% Ending star matches any string that begins with the part 
    %% before the star.
    ?assertEqual(true,  oni_match:verb(<<"foo">>, <<"foo*">>)),
    ?assertEqual(true,  oni_match:verb(<<"foobar">>, <<"foo*">>)),
    ?assertEqual(true,  oni_match:verb(<<"food">>, <<"foo*">>)),
    ?assertEqual(true,  oni_match:verb(<<"foogleman">>, <<"foo*">>)),
    %% Single star matches anything at all.
    ?assertEqual(true,  oni_match:verb(<<"quux">>, <<"*">>)),
    ?assertEqual(true,  oni_match:verb(<<"foo">>, <<"*">>)),
    ?assertEqual(true,  oni_match:verb(<<"bar">>, <<"*">>)).