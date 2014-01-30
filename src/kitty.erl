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
    oni_db:rename(Player, <<"generic player">>),

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

    Room1 = oni_db:create(nothing),
    oni_db:rename(Room1, <<"The First Room">>),
    oni_db:add_property(Room1, exits, []),

    Room2 = oni_db:create(nothing),
    oni_db:rename(Room2, <<"The Second Room">>),
    oni_db:add_property(Room2, exits, []),

    connect_rooms(Room1, Room2, fun(ThisSide, OtherSide) ->
            oni_db:rename(ThisSide, <<"east">>),
            oni_db:rename(OtherSide, <<"west">>)
        end),
    
    oni_db:move(Mistress, Room1),
    oni_db:move(Wizard, Room1),
    oni_db:move(Alana, Room1),

    oni_db:add_verb(Player, {Mistress, [<<"l*ook>">>]}, {none, none, none}),
    oni_db:set_verb_code(Player, 1, {kitty, look}),

    oni_db:add_verb(Player, {Mistress, [<<"go">>]}, {any, none, none}),
    oni_db:set_verb_code(Player, 1, {kitty, go}),

    oni_db:add_verb(Alana, {Mistress, [<<"speak">>]}, {none, none, none}),
    oni_db:set_verb_code(Alana, 1, {kitty, speak}).    

%%%============================================================================
%%% Actors
%%%============================================================================
alana_loop(Obj) ->
    %% Set player to 0 otherwise notify will fail
    Bindings = [{this, Obj}, {player, Obj}],
    next_action(Bindings),
    timer:sleep(25000),
    alana_loop(Obj).

spawn_alana(Obj) ->
    oni_aq_sup:start_queue(Obj),
    spawn(fun() -> alana_loop(Obj) end).

%% TODO: This just returns the first exit found
random_exit(Room) ->
    case oni_db:get_value(Room, exits) of
        [Exit|_] -> Exit;
        _ -> nothing
    end.

next_action(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    R = random:uniform(),
    case R of 
        R when R > 0.800 ->
            oni:announce(Location, <<"Prrrr.">>);
        R when R > 0.600 ->
            oni:announce(Location, <<"Alana looks at you ponderingly.">>);
        R when R > 0.400 ->
            case random_exit(Location) of
                nothing -> ok;
                Exit -> 
                    io:format("Moving Alana to ~p~n", [Exit]),
                    NewBindings = [{dobj, Exit}|Bindings],
                    Pack = oni_pack:create({kitty, start_go}, NewBindings),
                    oni_aq_sup:queue(This, Pack)
            end;
        R when R > 0.200 ->
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
%%% Player verbs
%%%============================================================================

look(Bindings) ->
    %% io:format("~p~n", [Bindings]),
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, "You look around.").

go(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Pack = oni_pack:create({kitty, start_go}, Bindings),
    oni_aq_sup:queue(Player, Pack).

start_go(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Dir = proplists:get_value(dobjstr, Bindings),
    Location = oni_db:location(Player),
    case find_exit(Location, Dir) of
        {ambiguous, _} ->
            oni:notify(Player, "I don't know which way '~s' you mean.", [Dir]);
        failed ->
            oni:notify(Player, "You can't go that way.");
        {_, Name} ->
            oni:notify(Player, "You go ~s", [Name]),
            {continue, 1500, {kitty, finish_go, [Bindings]}}
    end.

finish_go(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Dir = proplists:get_value(dobjstr, Bindings),
    Location = oni_db:location(Player),
    case find_exit(Location, Dir) of
        {ambiguous, _} ->
            oni:notify(Player, "You suddenly are confused about which way '~s' to go.", [Dir]);
        failed ->
            oni:notify(Player, "You can no longer go that way.");
        {Id, _} ->
            OtherSide = oni_db:get_value(Id, other_side),
            OtherRoom = oni_db:location(OtherSide),
            oni_db:move(Player, OtherRoom),
            oni:notify(Player, oni_db:name(OtherRoom))            
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec find_exit(oni_db:objid(), binary()) -> 
    {oni_db:objid(), binary()} | failed | {ambiguous, [binary()]}.
find_exit(Location, Name) ->
    ExitNames = exit_names(Location),
    oni_match:list(fun({_, X}) -> oni_bstr:starts_with(Name, X) end, ExitNames).

exit_names(Room) ->
    case oni_db:get_value(Room, exits) of
        'E_PROPNF' -> [];
        Exits -> exit_names(Exits, [])
    end.

exit_names([], Acc) -> lists:reverse(Acc);
exit_names([H|T], Acc) -> exit_names(T, [{H, oni_db:name(H)}|Acc]).

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