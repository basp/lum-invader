%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_db).

-compile(export_all).

-type objid() :: integer() | nothing.

-record(object, {id, 
                 parent = nothing, 
                 name = <<"">>,
                 location = nothing, 
                 props = [], 
                 verbs = [], 
                 flags = 0}).

-define(TABLE_OBJECTS, oni_objects).
-define(TABLE_COUNTERS, oni_counters).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Initializes the object table.
%%-----------------------------------------------------------------------------
init() ->
	%% Stores all the objects that we know about
	ets:new(?TABLE_OBJECTS, [ordered_set, {keypos, #object.id}, named_table, public]),
	ets:insert(?TABLE_OBJECTS, #object{id = 0}),
	%% Keeps track of max object id and any other counters
	ets:new(?TABLE_COUNTERS, [set, named_table, public]),
	ets:insert(?TABLE_COUNTERS, {max_id, 0}).

%%-----------------------------------------------------------------------------
%% @doc Creates a new object and returns the id of the newly created object.
%%-----------------------------------------------------------------------------
-spec create(Parent::objid()) -> objid().
create(Parent) ->
	Id = update_counter(max_id, 1),
	ets:insert(?TABLE_OBJECTS, #object{id = Id, parent = Parent}),
	Id.

%%-----------------------------------------------------------------------------
%% @doc Determines whether the given object id belongs to an existing object.
%%-----------------------------------------------------------------------------
-spec valid(Id::objid()) -> boolean().
valid(Id) when Id < 0 -> 
	false;
valid(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> false;
		_ -> true
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the name of object with id.
%%-----------------------------------------------------------------------------
-spec name(Id::objid()) -> binary().
name(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.name
	end.

-spec rename(Id::objid(), Name::binary()) -> 'E_INVARG' | true.
rename(Id, Name) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			ets:insert(?TABLE_OBJECTS, Obj#object{name = Name})
	end.

%%-----------------------------------------------------------------------------
%% @doc Changes the parent of the object with specified id.
%%-----------------------------------------------------------------------------
-spec chparent(Id::objid(), NewParent::objid()) -> true | 'E_INVARG'.
chparent(Id, NewParent) when NewParent =:= nothing ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> ets:insert(?TABLE_OBJECTS, Obj#object{parent = NewParent})
	end;
chparent(Id, NewParent) ->
	case {ets:lookup(?TABLE_OBJECTS, Id), valid(NewParent)} of
		{[], _} -> 'E_INVARG';
		{_, false} -> 'E_INVARG';
		{[Obj], _} -> ets:insert(?TABLE_OBJECTS, Obj#object{parent = NewParent})
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the parent of the object with specified id.
%%-----------------------------------------------------------------------------
-spec parent(Id::objid()) -> objid().
parent(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.parent
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the objects that have their parent set to specified id.
%%-----------------------------------------------------------------------------
-spec children(Id::objid()) -> [objid()] | 'E_INVARG'.
children(Id) ->
	case valid(Id) of 
		false -> ' E_INVARG';
		true ->
			M = [{#object{id = '$1', parent = Id, _='_'}, [], ['$1']}],
			ets:select(?TABLE_OBJECTS, M)
	end.

%%-----------------------------------------------------------------------------
%% @doc Destroys the object with specified id.
%%
%% The contents of the destroyed object will have their location set to
%% nothing after this operation completes. 
%%
%% Note that we explicitly don't manipulate the max_id counter here - we want 
%% old ids to point to invalid objects and not to have them silently replaced
%% by a completely new object with an old id.
%%-----------------------------------------------------------------------------
-spec recycle(Id::objid()) -> true | 'E_INVARG'.
recycle(Id) ->
	case valid(Id) of
		false -> 'E_INVARG';
		true -> 
			lists:foreach(fun(X) -> move(X, nothing) end, contents(Id)),
			ets:delete(?TABLE_OBJECTS, Id)
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the largest object id assigned to an object.
%%-----------------------------------------------------------------------------
-spec max_object() -> objid().
max_object() ->
	case ets:lookup(?TABLE_COUNTERS, max_id) of
		[] -> nothing;
		[{max_id, Id}] -> Id
	end.

%%-----------------------------------------------------------------------------
%% @doc Moves what to where.
%%-----------------------------------------------------------------------------
-spec move(What::objid(), Where::objid()) -> true | 'E_INVARG'.
move(What, Where) when Where =:= nothing ->
	case ets:lookup(?TABLE_OBJECTS, What) of
		[] -> 'E_INVARG';
		[Obj] -> ets:insert(?TABLE_OBJECTS, Obj#object{location = Where})
	end;
move(What, Where) ->
	case {ets:lookup(?TABLE_OBJECTS, What), valid(Where)} of
		{[], _} -> 'E_INVARG';
		{_, false} -> 'E_INVARG';
		{[Obj], _} -> ets:insert(?TABLE_OBJECTS, Obj#object{location = Where})
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the location of the object with specified id.
%%-----------------------------------------------------------------------------
-spec location(Id::objid()) -> objid().
location(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.location
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the objects that have their location set to specified id.
%%-----------------------------------------------------------------------------
-spec contents(Id::objid()) ->	[objid()] | 'E_INVARG'.
contents(Id) ->
	case valid(Id) of
		false -> 'E_INVARG';
		true ->
			M = [{#object{id = '$1', location = Id, _='_'}, [], ['$1']}],
			ets:select(?TABLE_OBJECTS, M)
	end.


%%-----------------------------------------------------------------------------
%% @doc Returns a list of all (non-builtin) properties.
%%-----------------------------------------------------------------------------
-spec properties(Id::objid()) -> [atom()] | 'E_INVARG'.
properties(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> proplists:get_keys(Obj#object.props)
	end.

%%-----------------------------------------------------------------------------
%% @doc Adds a property with Key and Value to object with Id.
%%-----------------------------------------------------------------------------
add_property(Id, Key, Value) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case lists:keyfind(Key, 1, Obj#object.props) of
				false ->
					NewProps = [{Key, {Value, []}}|Obj#object.props],
					ets:insert(?TABLE_OBJECTS, Obj#object{props = NewProps});
				_ -> 'E_INVARG'
			end
	end.

%%-----------------------------------------------------------------------------
%% @doc Returns the value of the object's property with specified key.
%%-----------------------------------------------------------------------------
-spec get_value(Id::objid(), Key::atom()) ->
	'E_INVARG' | 'E_PROPNF' | any().
get_value(Id, id) -> Id;
get_value(Id, parent) -> parent(Id);
get_value(Id, location) -> location(Id);
get_value(Id, contents) -> contents(Id);
get_value(Id, children) -> children(Id);
get_value(Id, name) -> name(Id);
get_value(Id, Key) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> 
			case lists:keyfind(Key, 1, Obj#object.props) of 
				false -> 'E_PROPNF';
				{_Key, {V, _Info}} -> V
			end
	end.

%%-----------------------------------------------------------------------------
%% @doc Set's the value of the object's property with specified key.
%%
%% We'll return 'E_INVARG' if someone tries to set readonly properties. These
%% properties should be manipulated with API functions in this module instead.
%%-----------------------------------------------------------------------------
-spec set_value(Id::objid(), Key::atom(), Value::any()) -> 
	'E_INVARG' | 'E_PROPNF' | any().
set_value(_Id, id, _Value) -> 'E_INVARG';
set_value(_Id, parent, _Value) -> 'E_INVARG';
set_value(_Id, location, _Value) -> 'E_INVARG';
set_value(_Id, contents, _Value) -> 'E_INVARG';
set_value(_Id, children, _Value) -> 'E_INVARG';
set_value(Id, name, Value) -> rename(Id, Value);
set_value(Id, Key, Value) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case lists:keyfind(Key, 1, Obj#object.props) of
				false -> 'E_PROPNF';
				{_Key, {_V, Info}} -> 
					lists:keyreplace(Key, 1, {Key, {Value, Info}})
			end
	end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

%% Used internally to keep track of object ids
-spec update_counter(Key::atom(), Incr::integer()) -> integer().
update_counter(Key, Incr) ->
	ets:update_counter(?TABLE_COUNTERS, Key, Incr).