%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test() ->
    ?assertEqual(
        {"take", "2.furax assembly", "with", "3.robot arm"},
        oni_parse:string("take 2.furax assembly with 3.robot arm")).

match_test() ->
    ?assertEqual("foo", oni_match:string("foo", ["foo"])),
    ?assertEqual("foo", oni_match:string("foo", ["foo", "bar", "quux"])),
    ?assertEqual(nothing, oni_match:string("foo", [])),
    ?assertEqual(nothing, oni_match:string("foo", ["bar", "quux"])),
    ?assertEqual(ambiguous, oni_match:string("foo", ["foo1", "foo2"])).

first_test() ->
    ?assertEqual(2, 
        oni_lists:first(fun(X) -> X =:= 2 end, [1,2,3])),
    ?assertEqual(foo,
        oni_lists:first(fun(X) -> X =:= foo end, [quux, bar, foo])).

scan_test() ->
    ?assertEqual(
        ["foo", "bar", "quux"], oni_parse:tokens("foo bar quux")),
    ?assertEqual(
        ["foo", "bar quux"], oni_parse:tokens("foo \"bar quux\"")).