%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Operations to wrap up things into runtime packages.
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
-module(oni_pack).

-export([cmd/2, code/1, bindings/1, match_verb_args/3]).

-record(package, {code = none, bindings = []}).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Get code from package.
code(#package{code = Code}) -> Code.

%% @doc Get bindings from package.
bindings(#package{bindings = Bindings}) -> Bindings.

%% @doc Package up a parsed command into something that can
%% be sent to the runtime.
%% @end
cmd(Cmd, User) ->
    Location = oni_db:location(User),
    Dobj = oni_cmd:dobj(Cmd),
    Prepstr = oni_cmd:prepstr(Cmd),
    Iobj = oni_cmd:iobj(Cmd),
    Verbstr = oni_cmd:verbstr(Cmd),
    Objects = [User, Location, Dobj, Iobj],
    Args = {Dobj, Prepstr, Iobj},
    case lookup_verb(Verbstr, Objects, Args) of
        none -> {error, Cmd}; %% should probably invoke "huh"
        {This, Code} ->
            Bindings = [{player, User},
                        {this, This},
                        {caller, User},
                        {verb, Verbstr},
                        {argstr, oni_cmd:argstr(Cmd)},
                        {args, oni_cmd:args(Cmd)},
                        {dobjstr, oni_cmd:dobjstr(Cmd)},
                        {dobj, Dobj},
                        {prepstr, Prepstr},
                        {iobjstr, oni_cmd:iobjstr(Cmd)},
                        {iobj, oni_cmd:iobj(Cmd)}],
            #package{code = Code, bindings = Bindings}
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

lookup_verb(_Str, [], _Args) -> none;
lookup_verb(Str, [H|T], Args) ->
    case oni_db:verbs(H) of
        'E_INVARG' -> lookup_verb(Str, T, Args);
        Names -> 
            case match_verb_name(Str, H, Args, Names, 1) of
                false -> lookup_verb(Str, T, Args);
                X -> X
            end
    end.

match_verb_name(_Str, _Obj, _Args, [], _Index) -> false;
match_verb_name(Str, Obj, Args, [H|T], Index) ->
    case oni_match:verb(Str, H) of
        false -> match_verb_name(Str, Obj, Args, T, Index + 1);
        true -> 
            Argspec = oni_db:verb_args(Obj, Index),
            case match_verb_args(Obj, Argspec, Args) of
                true -> {Obj, oni_db:verb_code(Obj, Index)};
                false -> match_verb_name(Str, Obj, Args, T, Index + 1)
            end
    end.

%% Dobj and Iobj specs: none, any, this.
%% Prep spec: none, any or one of the preps from the oni_cmd module
match_verb_args(_This, {none, none, none}, {Dobj, Prep, Iobj})
    when Dobj =:= nothing, Prep =:= <<>>, Iobj =:= nothing -> true;
match_verb_args(This, {this, any, this}, {Dobj, Prep, Iobj})
    when Dobj =:= This, Prep =/= <<>>, Iobj =:= This -> true;
match_verb_args(This, {this, any, any}, {Dobj, Prep, Iobj})
    when Dobj =:= This, Prep =/= <<>>, Iobj =/= nothing -> true;
match_verb_args(This, {any, any, this}, {Dobj, Prep, Iobj})
    when Dobj =/= nothing, Prep =/= <<>>, Iobj =:= This -> true;
match_verb_args(_This, {any, none, none}, {Dobj, Prep, Iobj})
    when Dobj =/= nothing, Prep =:= <<>>, Iobj =:= nothing -> true;
match_verb_args(This, {this, none, none}, {Dobj, Prep, Iobj})
    when Dobj =:= This, Prep =:= <<>>, Iobj =:= nothing -> true;
match_verb_args(_This, {any, any, any}, {Dobj, Prep, Iobj})
    when Dobj =/= nothing, Prep =/= <<>>, Iobj =/= nothing -> true;
match_verb_args(_This, _, _) -> false.

