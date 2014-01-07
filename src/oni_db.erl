%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Object store functions.
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_db).

-compile(export_all).

-type objid() :: integer() | nothing.

-record(object, {id, 
	             parent = nothing, 
				 location = nothing, 
				 props = [], 
				 verbs = [], 
				 flags = 0}).

-define(TABLE, oni_objects).

%%-----------------------------------------------------------------------------
%% @doc Initializes the object table.
%%-----------------------------------------------------------------------------
init() ->
	ets:new(?TABLE, [ordered_set, {keypos, #object.id}, named_table]),
	ets:insert(?TABLE, #object{id = 0}).
	
%%-----------------------------------------------------------------------------
%% @doc Creates a new object and returns the id of the newly created object.
%%-----------------------------------------------------------------------------
-spec create(Parent::objid()) -> objid().
create(Parent) ->
	Id = max_object() + 1,
	ets:insert(?TABLE, #object{id = Id, parent = Parent}),
	Id.

%%-----------------------------------------------------------------------------
%% @doc Determines whether the given object id belongs to an existing object.
%%-----------------------------------------------------------------------------
-spec valid(Id::objid()) -> boolean().
valid(Id) when Id < 0 -> 
	false;
valid(Id) ->
	case ets:lookup(?TABLE, Id) of
		[] -> false;
		_ -> true
	end.

%%-----------------------------------------------------------------------------
%% @doc Changes the parent of the object with specified id.
%%-----------------------------------------------------------------------------
-spec chparent(Id::objid(), NewParent::objid()) -> true | 'E_INVARG'.
chparent(Id, NewParent) when NewParent =:= nothing ->
	case ets:lookup(?TABLE, Id) of
		[] -> 'E_INVARG';
		[Obj] -> ets:insert(?TABLE, Obj#object{parent = NewParent})
	end;
chparent(Id, NewParent) ->
	case {ets:lookup(?TABLE, Id), valid(NewParent)} of
		{[], _} -> 'E_INVARG';
		{_, false} -> 'E_INVARG';
		{[Obj], _} -> ets:insert(?TABLE, Obj#object{parent = NewParent})
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the parent of the object with specified id.
%%-----------------------------------------------------------------------------
-spec parent(Id::objid()) -> objid().
parent(Id) ->
	case ets:lookup(?TABLE, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.parent
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the objects that have their parent set to specified id.
%%-----------------------------------------------------------------------------
-spec children(Id::objid()) -> [objid()].
children(Id) ->
	MatchSpec = [{#object{id = '$1', parent = Id, _='_'}, [], ['$1']}],
	ets:select(?TABLE, MatchSpec).

%%-----------------------------------------------------------------------------
%% @doc Returns the location of the object with specified id.
%%-----------------------------------------------------------------------------
-spec location(Id::objid()) -> objid().
location(Id) ->
	case ets:lookup(?TABLE, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.location
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the objects that have their location set to specified id.
%%-----------------------------------------------------------------------------
-spec contents(Id::objid()) ->	[objid()].
contents(Id) ->
	MatchSpec = [{#object{id = '$1', location = Id, _='_'}, [], ['$1']}],
	ets:select(?TABLE, MatchSpec).

-spec recycle(Id::objid()) -> true | 'E_INVARG'.
recycle(Id) ->
	case valid(Id) of
		false -> 'E_INVARG';
		true -> 
			lists:foreach(fun(X) -> move(X, nothing) end, contents(Id)),
			ets:delete(Id)
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the largest object id assigned to an object.
%%-----------------------------------------------------------------------------
-spec max_object() -> objid().
max_object() ->
	case ets:last(?TABLE) of
		'$end_of_table' -> nothing;
		Id -> Id
	end.

%%-----------------------------------------------------------------------------
%% @doc Moves what to where.
%%-----------------------------------------------------------------------------
-spec move(What::objid(), Where::objid()) -> true | 'E_INVARG'.
move(What, Where) when Where =:= nothing ->
	case ets:lookup(?TABLE, What) of
		[] -> 'E_INVARG';
		[Obj] -> ets:insert(?TABLE, Obj#object{location = Where})
	end;
move(What, Where) ->
	case {ets:lookup(?TABLE, What), valid(Where)} of
		{[], _} -> 'E_INVARG';
		{_, false} -> 'E_INVARG';
		{[Obj], _} -> ets:insert(?TABLE, Obj#object{location = Where})
	end.