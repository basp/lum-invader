%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Sandbox world implementation.
%%%
%%% This is just a sample module used for tutorials and to show how to setup
%%% a basic world using nothing but the bare Oni core. It also shows off how
%%% to implement instant vs long running actions (look vs. survey).
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

-module(sandbox).
-compile(export_all).

init() ->
    Player = oni_db:create(nothing),
    oni_db:add_verb(Player, {Player, [<<"l*ook">>]}, {none, none, none}),
    oni_db:set_verb_code(Player, 1, {sandbox, look}),
    oni_db:add_verb(Player, {Player, [<<"surv*ey">>]}, {none, none, none}),
    oni_db:set_verb_code(Player, 1, {sandbox, survey}),
    Wizard = oni_db:create(Player),
    oni_db:rename(Wizard, <<"Wizard">>),
    oni_db:set_player_flag(Wizard, true),
    oni_db:set_wizard_flag(Wizard, true),
    Mistress = oni_db:create(Player),
    oni_db:rename(Mistress, <<"Mistress">>),
    oni_db:set_player_flag(Mistress, true),
    oni_db:set_wizard_flag(Mistress, true).

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
    {continue, 3000, {sandbox, continue_survey, [Bindings]}}.

continue_survey(Bindings) ->
    case random:uniform() > 0.555 of
        true ->
            Player = proplists:get_value(player, Bindings),
            oni:notify(Player, "You find some interesting terrain."),
            {continue, 5000, {sandbox, continue_survey, [Bindings]}};
        false ->
            {continue, 1000, {sandbox, finish_survey, [Bindings]}}
    end.

finish_survey(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, <<"You finish surveying your surroundings.">>).