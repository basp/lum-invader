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
-module(kitty_alana).

-compile(export_all).

create(Owner) ->
    Alana = oni_db:create(nothing),
    oni_db:rename(Alana, <<"Alana">>),
    oni_db:add_verb(Alana, {Owner, [<<"pet">>]}, {this, none, none}),
    oni_db:set_verb_code(Alana, 1, {kitty_alana, pet}),
    Alana.

%% @doc Actor loops should be started with the actor supervisor:
%% oni_actor_sup:start_actor(4, {kitty, alana_loop}).
%% @end
loop(Obj) ->
    %% Set player to 0 otherwise notify will fail
    Bindings = [{this, Obj}, {player, Obj}],
    next_action(Bindings),
    timer:sleep(25000),
    loop(Obj).

random_exit(Room) ->
    case Exits = oni_db:get_value(Room, exits) of
        [_|_] ->
            I = random:uniform(length(Exits)),
            lists:nth(I, Exits);
        _ -> nothing
    end.

next_action(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    R = random:uniform(),
    case R of 
        R when R > 0.8000 ->
            oni:announce(Location, <<"Prrrr.">>);
        R when R > 0.600 ->
            oni:announce(Location, <<"Alana looks at you ponderingly.">>);
        R when R > 0.400 ->
            case random_exit(Location) of
                nothing -> ok;
                Exit -> 
                    Name = oni_db:name(Exit),
                    NewBindings = [{dobjstr, Name}|Bindings],
                    Pack = oni_pack:create({kitty_player, start_go}, NewBindings),
                    oni_aq_sup:queue(This, Pack)
            end;
        R when R > 0.200 ->
            Pack = oni_pack:create({kitty_alana, start_gnawing}, Bindings),
            oni_aq_sup:queue(This, Pack);
        _Other ->
            oni:announce(Location, <<"Miiiiiiiaaaaauuuuwwww!">>)
    end.

start_gnawing(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    oni:announce(Location, <<"Alana starts gnawing on some cables.">>),
    {continue, 3000, {kitty_alana, continue_gnawing, [Bindings]}}.

continue_gnawing(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    oni:announce(Location, <<"Alana continues to gnaw on some cables.">>),
    {continue, 1500, {kitty_alana, finish_gnawing, [Bindings]}}.

finish_gnawing(Bindings) ->
    This = proplists:get_value(this, Bindings),
    Location = oni_db:location(This),
    oni:announce(Location, <<"Alana finishes gnawing on his cable. He seems satisfied for now.">>).

pet(Bindings) ->
    Player = proplists:get_value(player, Bindings),
    oni:notify(Player, <<"Alana looks at you curiously.">>).