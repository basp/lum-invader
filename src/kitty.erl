-module(kitty).

-compile(export_all).

init() ->
    %% Player
    Player = oni_db:create(nothing),

    %% Mistress
    Mistress = oni_db:create(Player),
    oni_db:rename(Mistress, <<"Mistress">>),
    oni_db:set_wizard_flag(Mistress, true),
    oni_db:set_player_flag(Mistress, true),   

    Wizard = oni_db:create(Player),
    oni_db:rename(Wizard, <<"Wizard">>),
    oni_db:set_wizard_flag(Wizard, true),
    oni_db:set_player_flag(Wizard, true),

    Alana = oni_db:create(nothing),
    oni_db:rename(Alana, <<"Alana">>),

    Room = oni_db:create(nothing),
    oni_db:rename(Room, <<"The Void">>),

    oni_db:move(Mistress, Room),
    oni_db:move(Wizard, Room),
    oni_db:move(Alana, Room),

    oni_db:add_verb(Player, {Mistress, [<<"l*ook>">>]}, {none, none, none}),
    oni_db:set_verb_code(Player, 1, {kitty, look}),

    oni_db:add_verb(Alana, {Mistress, [<<"speak">>]}, {none, none, none}),
    oni_db:set_verb_code(Alana, 1, {kitty, speak}).    

%%%============================================================================
%%% Actors
%%%============================================================================
alana_loop(Obj) ->
    Bindings = [{this, Obj}],
    speak(Bindings),
    timer:sleep(4000),
    alana_loop(Obj).

spawn_alana(Obj) ->
    spawn(fun() -> alana_loop(Obj) end).

speak(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    R = random:uniform(),
    case R of 
        R when R > 0.800 ->
            oni:announce(Location, <<"Prrrr.">>);
        R when R > 0.500 ->
            oni:announce(Location, <<"Alana looks at you pondering.">>);
        _Other ->
            oni:announce(Location, <<"Miiiiiiiaaaaauuuuwwww!">>)
    end.

%%%============================================================================
%%% Mistress verbs
%%%============================================================================

look(Bindings) ->
    %% io:format("~p~n", [Bindings]),
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, "You look around.").

