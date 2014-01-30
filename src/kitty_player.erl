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
-module(kitty_player).

-compile(export_all).

create(Name, Owner) ->
    Id = oni_db:create(nothing),
    oni_db:rename(Id, Name),
    oni_db:set_player_flag(Id, true),

    oni_db:add_verb(Id, {Owner, [<<"l*ook>">>]}, {none, none, none}),
    oni_db:set_verb_code(Id, 1, {kitty_player, look}),

    oni_db:add_verb(Id, {Owner, [<<"g*o">>]}, {any, none, none}),
    oni_db:set_verb_code(Id, 1, {kitty_player, go}),

    oni_db:add_verb(Id, {Owner, [<<"k*ill">>]}, {any, none, none}),
    oni_db:set_verb_code(Id, 1, {kitty_player, attack}),

    oni_db:add_verb(Id, {Owner, [<<"@exits">>]}, {none, none, none}),
    oni_db:set_verb_code(Id, 1, {kitty_player, sys_exits}),

    Id.

sys_exits(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Location = oni_db:location(Player),
    oni:notify("~p", [exit_names(Location)]).

look(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    case oni_db:location(Player) of
        nothing -> oni:notify(Player, "You are nowhere!");
        Location -> oni:notify(Player, oni_db:name(Location))
    end.

go(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Pack = oni_pack:create({kitty_player, start_go}, Bindings),
    oni_aq_sup:queue(Player, Pack).

attack(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Pack = oni_pack:create({kitty_player, start_attack}, Bindings),
    oni_aq_sup:queue(Player, Pack).

start_attack(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    Target = proplists:get_value(dobj, Bindings),
    case Target =:= Player of
        true -> 
            oni:notify(Player, "This might not be the best way to kill yourself."), ok;
        false -> 
            oni:notify(Player, "You start attacking ~p", [oni_db:name(Target)]),
            {continue, 500, {kitty_player, finish_attack, [Bindings]}}
    end.

finish_attack(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, "You feel exausted, maybe you're not cut out for this fighting business.").

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
            {continue, 1500, {kitty_player, finish_go, [Bindings]}}
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