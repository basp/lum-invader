%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Basic world implementation and test functions.
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
-module(oni_test_world).

-compile(export_all).

look(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    case oni_db:location(Player) of
        nothing -> oni:notify(Player, <<"You are nowhere!">>);
        Location -> oni:notify(Player, format_room_description(Location))
    end,
    {done, Bindings}.

look_object(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, <<"You look at something.">>),
    {done, Bindings}.

format_room_description(Id) ->
    Name = oni_db:name(Id),
    <<"You are in ", Name/binary>>.
    
