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

%% Create a basic database with two wizard players in a room. 
%% All objects will have a basic description property setup.
create() ->
    Wizard = oni_db:create(nothing),
    true = oni_db:set_wizard_flag(Wizard, true),
    true = oni_db:set_player_flag(Wizard, true),
    true = oni_db:set_programmer_flag(Wizard, true),
    true = oni_db:rename(Wizard, <<"Wizard">>),
    true = oni_db:add_property(Wizard, description, <<"You are not sure.">>),

    Mistress = oni_db:create(Wizard),
    true = oni_db:set_wizard_flag(Mistress, true),
    true = oni_db:set_player_flag(Mistress, true),
    true = oni_db:set_programmer_flag(Mistress, true),
    true = oni_db:rename(Mistress, <<"Mistress">>),
    true = oni_db:add_property(Mistress, description, <<"She seems quite fierce.">>),

    Room = oni_db:create(nothing),
    true = oni_db:rename(Room, <<"The First Room">>),
    true = oni_db:add_property(Room, description, <<"It's the first (and only) room.">>),

    true = oni_db:move(Wizard, Room),
    true = oni_db:move(Mistress, Room),

    true = oni_db:add_verb(Wizard, {Wizard, [<<"@ex*amine">>]}, {none, none, none}),
    true = oni_db:set_verb_code(Wizard, 1, {oni_test_world, examine}),

    true = oni_db:add_verb(Wizard, {Wizard, [<<"@ex*amine">>]}, {any, none, none}),
    true = oni_db:set_verb_code(Wizard, 1, {oni_test_world, examine_object}),

    true = oni_db:add_verb(Mistress, {Wizard, [<<"l*ook">>]}, {none, none, none}),
    true = oni_db:set_verb_code(Mistress, 1, {oni_test_world, look}),

    true = oni_db:add_verb(Mistress, {Wizard, [<<"l*ook">>]}, {any, none, none}),
    true = oni_db:set_verb_code(Mistress, 1, {oni_test_world, look_object}).

examine(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Location = oni_db:location(Player),
    Verbs = oni_db:verbs(Location),
    oni:notify(Player, "~p", [Verbs]),
    {done, Bindings}.

examine_object(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Dobj = proplists:get_value(dobj, Bindings),
    Verbs = oni_db:verbs(Dobj),
    oni:notify(Player, "~p", [Verbs]),
    {done, Bindings}.

look(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    case oni_db:location(Player) of
        nothing -> oni:notify(Player, <<"You are nowhere!">>);
        Location -> oni:notify(Player, format_room_description(Location))
    end,
    {done, Bindings}.

look_object(Bindings) ->
    Thing = proplists:get_value(dobj, Bindings),
    {done, look_self(Thing, Bindings)}.

look_self(Self, Bindings) ->
    Player = proplists:get_value(player, Bindings),
    case oni_db:get_value(Self, description) of
        Description when is_binary(Description) ->
            oni:notify(Player, Description);
        X ->
            case {oni_db:is_wizard(Player), oni_db:is_programmer(Player)} of
                {true, _} -> oni:debug(Player, [X, Bindings]);
                {_, true} -> oni:debug(Player, [X, Bindings]);
                _Else -> oni:notify("You can't really make out what it is.")
            end
    end,
    Bindings.

format_room_description(Id) ->
    Name = oni_db:name(Id),
    <<"You are in ", Name/binary>>.

start_foo(Bindings) ->
    io:format("You start fooing.~n"),
    {continue, 2000, {oni_test_world, finish_foo, [Bindings]}}.

finish_foo(_Bindings) ->
    io:format("You finish fooing.~n").