%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Part of the kitty example implementation.
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
-module(kitty).

-compile(export_all).

init() ->
    %% Mistress
    Mistress = kitty_player:create(<<"Mistress">>, nothing),
    oni_db:set_wizard_flag(Mistress, true),

    Wizard = kitty_player:create(<<"Wizard">>, nothing),
    oni_db:set_wizard_flag(Wizard, true),

    Room1 = oni_db:create(nothing),
    oni_db:rename(Room1, <<"The First Room">>),
    oni_db:add_property(Room1, exits, []),

    Room2 = oni_db:create(nothing),
    oni_db:rename(Room2, <<"The Second Room">>),
    oni_db:add_property(Room2, exits, []),

    Room3 = oni_db:create(nothing),
    oni_db:rename(Room3, <<"The Third Room">>),
    oni_db:add_property(Room3, exits, []),

    Alana = kitty_alana:create(Mistress),

    connect_rooms(Room1, Room2, fun(ThisSide, OtherSide) ->
            oni_db:rename(ThisSide, <<"east">>),

            oni_db:add_property(ThisSide, oleave_msg, <<"%n walks to the east.">>),
            oni_db:add_verb(ThisSide, {Mistress, [<<"oleave_msg">>]}, {this, none, this}),
            oni_db:set_verb_code(ThisSide, 1, {kitty, exit_oleave}),

            oni_db:add_property(ThisSide, oarrive_msg, <<"%n arrives from the east.">>),
            oni_db:add_verb(ThisSide, {Mistress, [<<"oarrive_msg">>]}, {this, none, this}),
            oni_db:set_verb_code(ThisSide, 1, {kitty, exit_oarrive}),

            oni_db:rename(OtherSide, <<"west">>),

            oni_db:add_property(OtherSide, oleave_msg, <<"%n walks to the west.">>),
            oni_db:add_verb(OtherSide, {Mistress, [<<"oleave_msg">>]}, {this, none, this}),
            oni_db:set_verb_code(OtherSide, 1, {kitty, exit_oleave}),

            oni_db:add_property(OtherSide, oarrive_msg, <<"%n arrives from the west.">>),
            oni_db:add_verb(OtherSide, {Mistress, [<<"oarrive_msg">>]}, {this, none, this}),
            oni_db:set_verb_code(OtherSide, 1, {kitty, exit_oarrive})
        end),

    connect_rooms(Room2, Room3, fun(ThisSide, OtherSide) ->
            oni_db:rename(ThisSide, <<"down">>),

            oni_db:add_property(ThisSide, oleave_msg, <<"%n crawls through the hatch.">>),
            oni_db:add_verb(ThisSide, {Mistress, [<<"oleave_msg">>]}, {this, none, tthis}),
            oni_db:set_verb_code(ThisSide, 1, {kitty, exit_oleave}),

            oni_db:add_property(ThisSide, oarrive_msg, <<"%n arrives from the hatch.">>),
            oni_db:add_verb(ThisSide, {Mistress, [<<"oarrive_msg">>]}, {this, none, this}),
            oni_db:set_verb_code(ThisSide, 1, {kitty, exit_oarrive}),

            oni_db:rename(OtherSide, <<"up">>),

            oni_db:add_property(ThisSide, oleave_msg, <<"%n climbs through the hatch.">>),
            oni_db:add_verb(ThisSide, {Mistress, [<<"oleave_msg">>]}, {this, none, tthis}),
            oni_db:set_verb_code(ThisSide, 1, {kitty, exit_oleave}),

            oni_db:add_property(ThisSide, oarrive_msg, <<"%n arrives from the hatch.">>),
            oni_db:add_verb(ThisSide, {Mistress, [<<"oarrive_msg">>]}, {this, none, this}),
            oni_db:set_verb_code(ThisSide, 1, {kitty, exit_oarrive})
        end),
    
    oni_db:move(Mistress, Room1),
    oni_db:move(Wizard, Room1),
    oni_db:move(Alana, Room1),

    oni_actor_sup:start_actor(Alana, {kitty_alana, loop}).

voices(Obj) ->
    case random:uniform() of
        X when X > 0.666 ->
            oni:notify(Obj, "Someone whispers, \"You there, I can see you.\"");
        X when X > 0.333 ->
            oni:notify(Obj, "Someone whispers, \"Stop ignoring me and listen.\"");
        _Else ->
            oni:notify(Obj, "You think you hear faint whispers.")
    end,
    timer:sleep(45000),
    voices(Obj).

%%%============================================================================
%%% Internal functions
%%%============================================================================

connect_rooms(From, To, SetupExits) ->
    ThisSide = create_exit(From),
    OtherSide = create_exit(To),
    oni_db:add_property(ThisSide, other_side, OtherSide),
    oni_db:add_property(OtherSide, other_side, ThisSide),
    SetupExits(ThisSide, OtherSide).

create_exit(Room) ->
    Exit = oni_db:create(nothing),
    oni_db:move(Exit, Room),
    Rest = oni_db:get_value(Room, exits),
    oni_db:set_value(Room, exits, [Exit|Rest]),
    Exit.