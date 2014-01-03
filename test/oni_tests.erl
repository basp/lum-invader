-module(oni_tests).

-include_lib("eunit/include/eunit.hrl").

starts_with_test() ->
    ?assertEqual(true, oni:starts_with("\"'", "\"foo")),
    ?assertEqual(true, oni:starts_with("\"'", "'foo")),
    ?assertEqual(false, oni:starts_with("\"'", "foo")).

ends_with_test() ->
    ?assertEqual(true, oni:ends_with("\"'", "foo\"")),
    ?assertEqual(true, oni:ends_with("\"'", "foo'")),
    ?assertEqual(false, oni:ends_with("\"'", "foo")).

between_test() ->
    ?assertEqual({true, false}, oni:between("\"'", "\"foo")),
    ?assertEqual({false, true}, oni:between("\"'", "foo\"")),
    ?assertEqual({true, true}, oni:between("\"'", "'foo\"")),
    ?assertEqual({true, true}, oni:between("\"'", "\"foo'")),
    ?assertEqual({true, true}, oni:between("\"'", "\"foo\"")),   
    ?assertEqual({true, true}, oni:between("\"'", "'foo'")),   
    ?assertEqual({false, false}, oni:between("\"'", "foo")).

match_test() ->
    ?assertEqual("foo", oni:match("foo", ["foo"])),
    ?assertEqual("foo", oni:match("foo", ["foo", "bar", "quux"])),
    ?assertEqual(nothing, oni:match("foo", [])),
    ?assertEqual(nothing, oni:match("foo", ["bar", "quux"])),
    ?assertEqual(ambiguous, oni:match("foo", ["foo1", "foo2"])).

first_test() ->
    ?assertEqual(2, 
        oni:first(fun(X) -> X =:= 2 end, [1,2,3])),
    ?assertEqual(foo,
        oni:first(fun(X) -> X =:= foo end, [quux, bar, foo])).

tokenize_test() ->
    ?assertEqual(["foo", "bar mumble", "baz"],
        oni:tokens("foo \"bar mumble\" baz")),
    ?assertEqual(["foo", "bar mumble", "baz"],
        oni:tokens("foo 'bar mumble' baz")),
    ?assertEqual(["foo", "bar mumble baz"],
        oni:tokens("foo \"bar mumble baz")),
    ?assertEqual(["foo", "bar mumble baz"],
        oni:tokens("foo 'bar mumble baz")).

delimited_test() ->
    ?assertEqual({[], ["foo", "bar"]}, 
        oni:delimited("\"'", ["foo", "bar"])),
    ?assertEqual({["quux"], ["foo", "bar"]}, 
        oni:delimited("\"'", ["foo", "bar\"", "quux"])),
    ?assertEqual({["quux"], ["foo", "bar"]},
        oni:delimited("\"'", ["foo", "bar'", "quux"])).

parse_test() ->
    ?assertEqual({"foo", ["bar" ,"quux"], "bar quux"},
        oni:parse("foo bar quux")),
    ?assertEqual({"foo", ["bar quux"], "bar quux"},
        oni:parse("foo 'bar quux'")),
    ?assertEqual({"foo", ["bar quux"], "bar quux"},
        oni:parse("foo \"bar quux")),
    ?assertEqual({"foo", ["bar quux"], "bar quux"},
        oni:parse("foo 'bar quux")).