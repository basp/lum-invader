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
-module(oni_cmd_tests).

-include_lib("eunit/include/eunit.hrl").

p(Str) -> oni_cmd:parse(Str, nothing).

final_quote_test() ->
    ?assertEqual(<<"foo">>, oni_cmd:verbstr(p(<<"foo = \"bar\"">>))).

final_quote_with_single_dot_test() ->
    ?assertEqual(<<"foo">>, oni_cmd:verbstr(p(<<"foo = \"bar\".">>))).