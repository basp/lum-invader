-module(sandbox).
-compile(export_all).

look(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, "You look around.").

survey(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Pack = oni_pack:create({sandbox, start_survey}, Bindings),
    oni_aq_sup:queue(Player, Pack).

start_survey(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, <<"You start surveying your surroundings.">>),
    {continue, 3000, {sandbox, finish_survey, [Bindings]}}.

finish_survey(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, <<"You finish surveying your surroundings.">>).