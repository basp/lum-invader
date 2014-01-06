%%%----------------------------------------------------------------------------
%%% @author Bas Pennings
%%%  [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_lists).

-export([first/2]).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Returns the first element satisfying the predicate.
%% See also http://stackoverflow.com/a/12657896.
%% 
%% @spec first(fun((any()) -> boolean()), [any()]) -> any() | nothing
%%-----------------------------------------------------------------------------
first(Pred, List) ->
    case lists:dropwhile(fun(X) -> not Pred(X) end, List) of
        [] -> nothing;
        [X | _] -> X
    end.