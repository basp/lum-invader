%%%----------------------------------------------------------------------------
%%% @author Bas Pennings
%%%  [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_bstr_tests).

-include_lib("eunit/include/eunit.hrl").

trim_start_test() ->
	?assertEqual(<<"foo">>, oni_bstr:trim_start(<<"    foo">>)),
	?assertEqual(<<"foo">>, oni_bstr:trim_start(<<"foo">>)),
	?assertEqual(<<"foo bar">>, oni_bstr:trim_start(<<"   foo bar">>)),
	?assertEqual(<<"foo   ">>, oni_bstr:trim_start(<<"foo   ">>)).

trim_end_test() ->
	?assertEqual(<<"foo">>, oni_bstr:trim_end(<<"foo    ">>)),
	?assertEqual(<<"foo">>, oni_bstr:trim_end(<<"foo">>)),
	?assertEqual(<<"foo bar">>, oni_bstr:trim_end(<<"foo bar   ">>)),
	?assertEqual(<<"   foo">>, oni_bstr:trim_end(<<"   foo">>)).

join_test() ->
	?assertEqual(
		<<"foo">>, 
		oni_bstr:join([<<"foo">>])),
	?assertEqual(
		<<"\"foo bar\"">>, 
		oni_bstr:join([<<"\"foo bar\"">>])),
	?assertEqual(
		<<"foo and bar">>, 
		oni_bstr:join([<<"foo">>, <<"bar">>], <<", ">>, <<" and ">>)),
	?assertEqual(
		<<"foo, bar and quux">>, 
		oni_bstr:join(
			[<<"foo">>, <<"bar">>, <<"quux">>], 
			<<", ">>, <<" and ">>)).

starts_with_test() ->
	?assertEqual(true, oni_bstr:starts_with(<<>>, <<"quux">>)),
	?assertEqual(true, oni_bstr:starts_with(<<"foo">>, <<"foobar">>)),
	?assertEqual(true, oni_bstr:starts_with(<<"fo">>, <<"fo quux">>)),
	?assertEqual(false, oni_bstr:starts_with(<<"fo">>, <<"quux">>)).

match_test() ->
	?assertEqual(
		nothing, oni_bstr:match(<<"foo">>, [])),
	?assertEqual(
		nothing, oni_bstr:match(<<"foo">>, [<<"bar">>])),
	?assertEqual(
		<<"foo">>, oni_bstr:match(<<"foo">>, [<<"foo">>, <<"bar">>])),
	?assertEqual(
		<<"bar">>, oni_bstr:match(<<"bar">>, [<<"foo">>, <<"bar">>])),
	?assertEqual(
		<<"foo">>, oni_bstr:match(<<"fo">>, [<<"bar">>, <<"foo">>])),
	?assertEqual(
		{ambiguous, <<"foobar">>, <<"foo">>}, 
		oni_bstr:match(<<"foo">>, [<<"foobar">>, <<"foo">>])),
	?assertEqual(
		{ambiguous, <<"f">>, <<"f">>}, 
		oni_bstr:match(<<"f">>, [<<"f">>, <<"f">>])).