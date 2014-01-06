%%%----------------------------------------------------------------------------
%%% @author Bas Pennings
%%%  [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Utility functions for working with binary strings.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_bstr).

-export([join/1, join/2, join/3, ps/2, trim_start/1, trim_end/1, trim/1]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Removes beginning whitespace.
%% 
%% @spec trim_start(binary()) -> binary()
%%-----------------------------------------------------------------------------
trim_start(<<>>) -> 
	<<>>;
trim_start(<<C, Rest/binary>>) 
		when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
	trim_start(Rest);
trim_start(Data) ->
	Data.

%%-----------------------------------------------------------------------------
%% @doc Removes trailing whitespace.
%%
%% @spec trim_end(binary()) -> binary()
%%-----------------------------------------------------------------------------
trim_end(Data) ->
	trim_end(Data, <<>>, <<>>).

%%-----------------------------------------------------------------------------
%% @doc Removes beginning and trailing whitespace.
%%
%% @spec trim(binary()) -> binary()
%%-----------------------------------------------------------------------------
trim(Data) ->
	trim_end(trim_start(Data)).

%%-----------------------------------------------------------------------------
%% @doc Performs pronoun substitution.
%% 
%% @spec sub(Data::binary(), Who::[term()]) -> binary().
%%-----------------------------------------------------------------------------
ps(Data, Who) ->
	ps(Data, Who, <<>>).

%%-----------------------------------------------------------------------------
%% @doc Joins the binaries using a space character as separator.
%% 
%% @spec join([binary()]) -> binary()
%%-----------------------------------------------------------------------------
join(Binaries) ->
	join(Binaries, <<$\s>>).

%%-----------------------------------------------------------------------------
%% @doc Joins the binaries using Sep as separator.
%% 
%% @spec join([binary()], Sep::binary()) -> binary()
%%-----------------------------------------------------------------------------
join(Binaries, Sep) ->
	join(Binaries, Sep, Sep).

%%-----------------------------------------------------------------------------
%% @doc Joins the binaries using LastSep for the last element.
%% 
%% @spec join([binary()], Sep::binary(), LastSep::binary()) -> binary()
%%-----------------------------------------------------------------------------
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

