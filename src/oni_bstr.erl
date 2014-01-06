%%%----------------------------------------------------------------------------
%%% @author Bas Pennings
%%%  [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Utility functions for working with binary strings.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_bstr).

-export([join/1, join/2, join/3, sub/2]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Performs pronoun substitution.
%% 
%% @spec sub(Data::binary(), Who::[term()]) -> binary().
%%-----------------------------------------------------------------------------
sub(Data, Who) ->
	sub(Data, Who, <<>>).

%%-----------------------------------------------------------------------------
%% @doc Joins the binaries using a space character as separator.
%% 
%% @spec join(Binaries::[binary()]) -> binary()
%%-----------------------------------------------------------------------------
join(Binaries) ->
	join(Binaries, <<$\s>>).

%%-----------------------------------------------------------------------------
%% @doc Joins the binaries using Sep as separator.
%% 
%% @spec join(Binaries::[binary()], Sep::binary()) -> binary()
%%-----------------------------------------------------------------------------
join(Binaries, Sep) ->
	join(Binaries, Sep, Sep).

%%-----------------------------------------------------------------------------
%% @doc Joins the binaries using Sep and LastSep for the last element.
%%
%% @spec join()
%%-----------------------------------------------------------------------------
join(Binaries, Sep, LastSep) ->
	join(Binaries, Sep, LastSep, <<>>).

%%%============================================================================
%%% Internal functions
%%%============================================================================

sub(<<>>, _Who, Acc) -> Acc;
sub(Data, Who, Acc) ->
	case Data of
		<<$%, $s, Rest/binary>> -> 
			V = get_prop(ps, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $o, Rest/binary>> ->
			V = get_prop(po, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $p, Rest/binary>> ->
			V = get_prop(pp, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $r, Rest/binary>> ->
			V = get_prop(pr, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $q, Rest/binary>> ->
			V = get_prop(pq, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $S, Rest/binary>> ->
			V = get_prop(psc, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $O, Rest/binary>> ->
			V = get_prop(poc, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $P, Rest/binary>> ->
			V = get_prop(ppc, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $R, Rest/binary>> ->
			V = get_prop(prc, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $Q, Rest/binary>> ->
			V = get_prop(pqc, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $n, Rest/binary>> ->
			V = get_prop(name, Who),
			sub(Rest, Who, <<Acc/binary, V/binary>>);
		<<$%, $%, Rest/binary>> ->
			sub(Rest, Who, <<Acc/binary, $%>>);
		<<C, Rest/binary>> -> 
			sub(Rest, Who, <<Acc/binary, C>>)
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

