%%%----------------------------------------------------------------------------
%%% @author Bas Pennings
%%%  [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_bstr_tests).

-include_lib("eunit/include/eunit.hrl").

join_test() ->
	?assertEqual(
		<<"foo">>, 
		oni_bstr:join([<<"foo">>])),
	?assertEqual(
		<<"\"foo bar\"">>, 
		oni_bstr:join([<<"\"foo bar\"">>])),
	?assertEqual(
		<<"foo and bar">>, 
		oni_bstr:join([<<"foo", "bar">>], <<", ">>, <<" and ">>)),
	?assertEqual(
		<<"foo, bar and quux">>, 
		oni_bstr:join(
			[<<"foo">>, <<"bar">>, <<"quux">>], 
			<<", ">>, <<" and ">>)).