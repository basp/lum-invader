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
-module(oni_ansi_tests).

-include_lib("eunit/include/eunit.hrl").

style_test() ->
	?assertEqual(<<27, $[, $0, $m>>, 		
		oni_ansi:style(<<"$reset;">>)),
	?assertEqual(<<27, $[, $1, $m>>,		
		oni_ansi:style(<<"$bold;">>)),
	?assertEqual(<<27, $[, $3, $m>>,		
		oni_ansi:style(<<"$italics;">>)),
	?assertEqual(<<27, $[, $4, $m>>,		
		oni_ansi:style(<<"$underline;">>)),
	?assertEqual(<<27, $[, $7, $m>>,		
		oni_ansi:style(<<"$inverse;">>)),
	?assertEqual(<<27, $[, $9, $m>>,		
		oni_ansi:style(<<"$strikethrough;">>)),
	?assertEqual(<<27, $[, $2, $2, $m>>,	
		oni_ansi:style(<<"$bold_off;">>)).

strip_test() ->
	?assertEqual(<<"">>, 
		oni_ansi:strip(<<"$reset;">>)),
	?assertEqual(<<"foo">>, 
		oni_ansi:strip(<<"$bold;foo$reset;">>)).