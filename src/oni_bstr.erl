%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Utility functions for working with binary strings.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_bstr).

-export([join/1, join/2, join/3, ps/2, 
	     trim_start/1, trim_end/1, trim/1,
	     starts_with/2, match/2]).

-type match() :: 
	any() | nothing | {ambiguous, First::binary(), Second::binary()}.

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Tries to match the prefix on a list of binary strings.
-spec match(Prefix::binary(), List::binary()) -> match().
match(Prefix, List) ->
	match(Prefix, List, nothing).

%% @doc Checks whether a binary starts with a particular prefix.
-spec starts_with(Prefix::binary(), Data::binary()) -> boolean().
starts_with(<<>>, _) ->
	true;
starts_with(_, <<>>) ->
	false;
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
%% @doc Performs pronoun substitution.
%%
%% %s 	ps 		subjective pronoun (he, she, it)
%% %o 	po 		objective pronoun (him, her, it)
%% %p   pp      possessive pronoun (his, her, its)   	
%% %r  	pr 		reflexive pronoun (himself, herself, itself)
%% %q   pq 		possessive (noun) (his, hers, its)
%% %S   psc     capitalized versions of the above
%% %O   poc
%% %P   ppc
%% %R   prc
%% %Q   pqc
%% %n 	name 	name of the player object
%% %%   %       double up to escape
%% @end
-spec ps(Data::binary(), Who::[term()]) -> binary().
ps(Data, Who) ->
	ps(Data, Who, <<>>).

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

match(<<>>, _List, _Found) -> nothing;
match(_Prefix, [], Found) -> Found;
match(Prefix, [H|T], Found) ->
	case starts_with(Prefix, H) of
		true when Found =/= nothing -> {ambiguous, Found, H};
		true -> match(Prefix, T, H);
		false -> match(Prefix, T, Found)
	end.

trim_end(<<>>, _Buffer, Acc) -> Acc;
trim_end(<<C, Rest/binary>>, Buffer, Acc)
	when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
		trim_end(Rest, <<Buffer/binary, C>>, Acc);
trim_end(<<C, Rest/binary>>, Buffer, Acc) ->
	trim_end(Rest, <<>>, <<Acc/binary, Buffer/binary, C>>).

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

join([X|Rest], Sep, LastSep, <<>>) ->
	join(Rest, Sep, LastSep, X);
join([X|[]], _Sep, LastSep, Acc) -> 
	<<Acc/binary, LastSep/binary, X/binary>>;
join([X|Rest], Sep, LastSep, Acc) ->
	join(Rest, Sep, LastSep, <<Acc/binary, Sep/binary, X/binary>>);
join([], _Sep, _LastSep, Acc) -> 
	Acc.