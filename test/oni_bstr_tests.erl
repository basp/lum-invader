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
-module(oni_bstr_tests).

-include_lib("eunit/include/eunit.hrl").

trim_start_test() ->
	?assertEqual(<<"foo">>, 	oni_bstr:trim_start(<<"    foo">>)),
	?assertEqual(<<"foo">>, 	oni_bstr:trim_start(<<"foo">>)),
	?assertEqual(<<"foo bar">>, oni_bstr:trim_start(<<"   foo bar">>)),
	?assertEqual(<<"foo   ">>, 	oni_bstr:trim_start(<<"foo   ">>)).

trim_end_test() ->
	?assertEqual(<<"foo">>, 	oni_bstr:trim_end(<<"foo    ">>)),
	?assertEqual(<<"foo">>, 	oni_bstr:trim_end(<<"foo">>)),
	?assertEqual(<<"foo bar">>, oni_bstr:trim_end(<<"foo bar   ">>)),
	?assertEqual(<<"   foo">>, 	oni_bstr:trim_end(<<"   foo">>)).

starts_with_test() ->
	?assertEqual(true, 		oni_bstr:starts_with(<<>>, <<"quux">>)),
	?assertEqual(true, 		oni_bstr:starts_with(<<"foo">>, <<"foobar">>)),
	?assertEqual(true, 		oni_bstr:starts_with(<<"fo">>, <<"fo quux">>)),
	?assertEqual(false, 	oni_bstr:starts_with(<<"fo">>, <<"quux">>)).

join_test() ->
	?assertEqual(<<"foo">>, 				
		oni_bstr:join([<<"foo">>])),
	?assertEqual(<<"\"foo bar\"">>, 		
		oni_bstr:join([<<"\"foo bar\"">>])),
	?assertEqual(<<"foo and bar">>, 		
		oni_bstr:join([<<"foo">>, <<"bar">>], <<", ">>, <<" and ">>)),
	?assertEqual(<<"foo, bar and quux">>, 	
		oni_bstr:join([<<"foo">>, <<"bar">>, <<"quux">>], <<", ">>, <<" and ">>)).