%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Kitty world implementation.
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
    %% Set player to 0 otherwise notify will fail
    Bindings = [{this, Obj}, {player, 0}],
    speak(Bindings),
    timer:sleep(4000),
    alana_loop(Obj).

spawn_alana(Obj) ->
    oni_aq_sup:start_queue(Obj),
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
        R when R > 0.250 ->
            Pack = oni_pack:create({kitty, start_gnawing}, Bindings),
            oni_aq_sup:queue(This, Pack);
        _Other ->
            oni:announce(Location, <<"Miiiiiiiaaaaauuuuwwww!">>)
    end.

start_gnawing(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    oni:announce(Location, <<"Alana starts gnawing on some cables.">>),
    {continue, 3000, {kitty, continue_gnawing, [Bindings]}}.

continue_gnawing(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    oni:announce(Location, <<"Alana continues to gnaw on some cables.">>),
    {continue, 1500, {kitty, finish_gnawing, [Bindings]}}.

finish_gnawing(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    oni:announce(Location, <<"Alana finishes gnawing on his cable. He seems satisfied for now.">>).

%%%============================================================================
%%% Mistress verbs
%%%============================================================================

look(Bindings) ->
    %% io:format("~p~n", [Bindings]),
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, "You look around.").