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

-export([cmd/2]).

-record(package, {code = none, bindings = []}).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Package up a parsed command into something that can
%% be sent to the runtime.
%% @end
cmd(Cmd, User) ->
    Location = oni_db:location(User),
    Dobj = oni_cmd:dobj(Cmd),
    Iobj = oni_cmd:iobj(Cmd),
    Verbstr = oni_cmd:verbstr(Cmd),
    Objects = [User, Location, Dobj, Iobj],
    case lookup_verb(Verbstr, Objects) of
        none -> Cmd; %% should probably invoke "huh"
        {This, Code} ->
            Bindings = [{player, User},
                        {this, This},
                        {caller, User},
                        {verb, Verbstr},
                        {argstr, oni_cmd:argstr(Cmd)},
                        {args, oni_cmd:args(Cmd)},
                        {dobjstr, oni_cmd:dobjstr(Cmd)},
                        {dobj, Dobj},
                        {prepstr, oni_cmd:prepstr(Cmd)},
                        {iobjstr, oni_cmd:iobjstr(Cmd)},
                        {iobj, oni_cmd:iobj(Cmd)}],
            #package{code = Code, bindings = Bindings}
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec lookup_verb(Str::binary(), [Obj::oni_cmd:objid()]) -> 
    {This::oni_cmd:objid(), {M::atom(), F::atom()}} | none.
lookup_verb(_Str, []) -> none;
lookup_verb(Str, [H|T]) ->
    case oni_db:verbs(H) of
        'E_INVARG' -> lookup_verb(Str, T);
        Names ->
            case lookup_verb_name(Str, Names) of
                false -> lookup_verb(Str, T);
                Name ->{H, oni_db:verb_code(H, Name)}
            end
    end.

lookup_verb_name(_Str, []) -> false;
lookup_verb_name(Str, [H|T]) ->
    case oni_match:verb(Str, H) of
        true -> H;
        false -> lookup_verb_name(Str, T)
    end.