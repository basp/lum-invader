-module(kitty).

-compile(export_all).

init() ->
    %% Create our initial wizard.
    Mistress = oni_db:create(nothing),
    oni_db:rename(Mistress, <<"Mistress">>),
    oni_db:set_wizard_flag(Mistress, true),
    oni_db:set_player_flag(Mistress, true),
    %% Setup basic verbs
    oni_db:add_verb(Mistress, {Mistress, [<<"l*ook>">>]}, {none, none, none}),
    oni_db:set_verb_code(Mistress, 1, {kitty, look}).

look() ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, "You look around.").