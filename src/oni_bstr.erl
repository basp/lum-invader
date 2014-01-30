%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Utility functions for working with binary strings.
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
-module(oni_bstr).

-export([join/1, join/2, join/3, 
	     trim_start/1, trim_end/1, trim/1,
	     starts_with/2, cap/1, ps/2]).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Capitalizes the string
-spec cap(Str::binary()) -> binary().
cap(<<C, Rest/binary>>) when C >= $a, C =< $z -> 
	<<(C - ($a - $A)), Rest/binary>>;
cap(X) -> X.

%% @doc Performs pronoun substitution.
%% ```
%% %s   ps      subjective pronoun (he, she, it)
%% %o   po      objective pronoun (him, her, it)
%% %p   pp      possessive pronoun (his, her, its)      
%% %r   pr      reflexive pronoun (himself, herself, itself)
%% %q   pq      possessive (noun) (his, hers, its)
%% %S   psc     capitalized versions of the above
%% %O   poc
%% %P   ppc
%% %R   prc
%% %Q   pqc
%% %n   name    name of the player object
%% %%   %       double up to escape
%% '''
%% @end
-spec ps(Data::binary(), Who::[term()]) -> binary().
ps(Data, Who) ->
    ps(Data, Who, <<>>).

%% @doc Checks whether a binary starts with a particular prefix.
%% Note that this ignores casing.
%% @end
-spec starts_with(Prefix::binary(), Data::binary()) -> boolean().
starts_with(<<>>, _) ->
	true;
starts_with(_, <<>>) ->
	false;
starts_with(<<X, RestX/binary>>, <<Y, RestY/binary>>)
    when X >= $A, X =< $Z, Y >= $a, Y =< $z, (X + 32) =:= Y ->
    starts_with(RestX, RestY); 
starts_with(<<X, RestX/binary>>, <<Y, RestY/binary>>)
    when X >= $a, X =< $z, Y >= $A, Y =< $Z, X =:= (Y + 32) ->
    starts_with(RestX, RestY);
starts_with(<<X, _/binary>>, <<Y, _/binary>>)
    when X =/= Y -> false;
starts_with(<<_, RestX/binary>>, <<_, RestY/binary>>) ->
	starts_with(RestX, RestY).

%% @doc Removes beginning whitespace.
-spec trim_start(Data::binary()) -> binary().
trim_start(<<>>) -> 
	<<>>;
trim_start(<<C, Rest/binary>>) 
	when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n -> trim_start(Rest);
trim_start(Data) ->
	Data.

%% @doc Removes trailing whitespace.
-spec trim_end(Data::binary()) -> binary().
trim_end(Data) ->
	trim_end(Data, <<>>, <<>>).

%% @doc Removes beginning and trailing whitespace.
-spec trim(Data::binary()) -> binary().
trim(Data) ->
	trim_end(trim_start(Data)).

%% @doc Joins the binaries using a space character as separator.
%%
%% This is used by the command parser to join back individual tokens and
%% normalize input. The join/2 and and join/3 functions are usually more
%% useful.
%% @end
-spec join(List::[binary()]) -> binary().
join(Binaries) ->
	join(Binaries, <<$\s>>).

%% @doc Joins the binaries using Sep as separator.
%%
%% Used to create nicely formatted lists such as "foo, bar, quux".
%% @end
-spec join(List::[binary()], Sep::binary()) -> binary().
join(Binaries, Sep) ->
	join(Binaries, Sep, Sep).

%% @doc Joins the binaries using LastSep for the last element.
%%
%% Useful to create even more nicely formatted lists such as "foo, bar 
%% and quux". Note the " and " token used as the LastSep in this
%% case.
%% @end
-spec join(List::[binary()], Sep::binary(), LastSep::binary()) -> binary().
join(Binaries, Sep, LastSep) ->
	join(Binaries, Sep, LastSep, <<>>).

%%%============================================================================
%%% Internal functions
%%%============================================================================

trim_end(<<>>, _Buffer, Acc) -> Acc;
trim_end(<<C, Rest/binary>>, Buffer, Acc)
	when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
		trim_end(Rest, <<Buffer/binary, C>>, Acc);
trim_end(<<C, Rest/binary>>, Buffer, Acc) ->
	trim_end(Rest, <<>>, <<Acc/binary, Buffer/binary, C>>).

join([X|Rest], Sep, LastSep, <<>>) ->
	join(Rest, Sep, LastSep, X);
join([X|[]], _Sep, LastSep, Acc) -> 
	<<Acc/binary, LastSep/binary, X/binary>>;
join([X|Rest], Sep, LastSep, Acc) ->
	join(Rest, Sep, LastSep, <<Acc/binary, Sep/binary, X/binary>>);
join([], _Sep, _LastSep, Acc) -> 
	Acc.

ps(<<>>, _Who, Acc) -> Acc;
ps(Data, Who, Acc) ->
    case Data of
        <<$%, $s, Rest/binary>> -> 
            V = get_prop(ps, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $o, Rest/binary>> ->
            V = get_prop(po, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $p, Rest/binary>> ->
            V = get_prop(pp, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $r, Rest/binary>> ->
            V = get_prop(pr, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $q, Rest/binary>> ->
            V = get_prop(pq, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $S, Rest/binary>> ->
            V = get_prop(psc, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $O, Rest/binary>> ->
            V = get_prop(poc, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $P, Rest/binary>> ->
            V = get_prop(ppc, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $R, Rest/binary>> ->
            V = get_prop(prc, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $Q, Rest/binary>> ->
            V = get_prop(pqc, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $n, Rest/binary>> ->
            V = get_prop(name, Who),
            ps(Rest, Who, <<Acc/binary, V/binary>>);
        <<$%, $%, Rest/binary>> ->
            ps(Rest, Who, <<Acc/binary, $%>>);
        <<C, Rest/binary>> -> 
            ps(Rest, Who, <<Acc/binary, C>>)
    end.

get_prop(Key, List) ->
    case proplists:get_value(Key, List) of
        undefined -> <<"undefined">>;
        X -> X
    end.