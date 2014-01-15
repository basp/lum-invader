%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Gender utilities.
%%%
%%% This is the API to manipulating objects and their properties.
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
-module(oni_gender).

-export([add/2, set/2, pronouns/0, genders/0, 
         ps/1, po/1, pp/1, pq/1, pr/1, psc/1, poc/1, ppc/1, pqc/1, prc/1]).

-type gender() :: male | female | neuter | plural.

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Adds pronoun properties if they are not already there.
-spec add(Obj::oni_db:objid(), Gender::gender()) -> gender().
add(Obj, Gender) ->
    add(Obj, Gender, pronouns()).

%% @doc Sets pronoun properties.
-spec set(Obj::oni_db:objid(), Gender::gender()) -> gender().
set(Obj, Gender) -> 
    set(Obj, Gender, pronouns()).

%% @doc Returns a list with pronoun properties.
-spec pronouns() -> [atom()].
pronouns() -> [ps, po, pp, pq, pr, psc, poc, ppc, pqc, prc].

%% @doc Returns a list of genders.
-spec genders() -> [gender()].
genders() -> [male, female, neuter, plural].

%% @doc Returns the subjective pronoun (e.g. he, she, it).
-spec ps(gender()) -> binary().
ps(male)    -> <<"he">>;
ps(female)  -> <<"she">>;
ps(neuter)  -> <<"it">>;
ps(plural)  -> <<"they">>.

%% @doc Returns the objective pronoun (e.g. him, her, it).
-spec po(gender()) -> binary().
po(male)    -> <<"him">>;
po(female)  -> <<"her">>;
po(neuter)  -> <<"it">>;
po(plural)  -> <<"them">>.

%% @doc Returns the possessive pronoun (e.g. his, her, its).
-spec pp(gender()) -> binary().
pp(male)    -> <<"his">>;
pp(female)  -> <<"her">>;
pp(neuter)  -> <<"its">>;
pp(plural)  -> <<"their">>.

%% @doc Returns the possessive pronoun in noun form (e.g. his, hers, its).
-spec pq(gender()) -> binary().
pq(male)    -> <<"his">>;
pq(female)  -> <<"hers">>;
pq(neuter)  -> <<"its">>;
pq(plural)  -> <<"theirs">>.

%% @doc Returns the reflexive pronoun (e.g. his, hers, its).
-spec pr(gender()) -> binary().
pr(male)    -> <<"himself">>;
pr(female)  -> <<"herself">>;
pr(neuter)  -> <<"itself">>;
pr(plural)  -> <<"themselves">>.

%% @doc Returns a capitalized version of the subjective pronoun.
-spec psc(gender()) -> binary().
psc(X) -> oni_bstr:cap(ps(X)).

%% @doc Returns a capitalized version of the objective pronoun.
-spec poc(gender()) -> binary().
poc(X) -> oni_bstr:cap(po(X)).

%% @doc Returns a capitalized version of the possessive pronoun.
-spec ppc(gender()) -> binary().
ppc(X) -> oni_bstr:cap(pp(X)).

%% @doc Returns a capitalized version of the possessive pronoun in noun form.
-spec pqc(gender()) -> binary().
pqc(X) -> oni_bstr:cap(pq(X)).

%% @doc Returns a capitalized version of the reflexive pronoun.
-spec prc(gender()) -> binary().
prc(X) -> oni_bstr:cap(pr(X)).

%%%============================================================================
%%% Internal functions
%%%============================================================================

add(_Obj, Gender, []) -> Gender;
add(Obj, Gender, [H|T]) ->
    oni_db:add_property(Obj, H, oni_gender:H(Gender)),
    add(Obj, Gender, T).

set(_Obj, Gender, []) -> Gender;
set(Obj, Gender, [H|T]) ->
    oni_db:set_value(Obj, H, oni_gender:H(Gender)),
    set(Obj, Gender, T).