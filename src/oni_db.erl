%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://themeticulousgeek.com]
%%% @copyright 2013-2014 Bas Pennings
%%% @doc Database API.
%%%
%%% TODO: This is becoming a bit unwieldy and verbose, it needs some TLC. 
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_db).

-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).

-record(object, {id, 
                 parent = nothing, 
                 name = <<"">>,
                 location = nothing, 
                 props = [], 
                 verbs = [], 
                 flags = 2#000000}).

%% Table identifiers
-define(TABLE_OBJECTS, 	oni_objects).
-define(TABLE_COUNTERS, oni_counters).

%% Object flags
-define(OBJECT_WIZARD, 	2#100000).
-define(OBJECT_PROG, 	2#010000).
-define(OBJECT_READ,	2#001000).
-define(OBJECT_WRITE,	2#000100).
-define(OBJECT_FERTILE,	2#000010).
-define(OBJECT_PLAYER,	2#000001).

-type objid() :: integer() | nothing.

%%%============================================================================
%%% Initializing the world database
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

%%%============================================================================
%%% Core object manipulation.
%%%============================================================================

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
%% @doc Returns a list of all objects that have the player flag set.
%%-----------------------------------------------------------------------------
-spec players() -> [objid()].
players() ->
	Q = qlc:q([O#object.id || 
		       O <- ets:table(?TABLE_OBJECTS), is_player(O#object.id)]),
	qlc:e(Q).

%%%============================================================================
%%% Manipulating object flags.
%%%============================================================================

-spec is_player(Id::objid()) -> boolean() | 'E_INVARG'.
is_player(Id) -> 
	is_object_flag_set(Id, ?OBJECT_PLAYER).

-spec set_player_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_player_flag(Id, Value) -> 
	update_object_flag(Id, ?OBJECT_PLAYER, Value).

-spec is_wizard(Id::objid()) -> boolean() | 'E_INVARG'.
is_wizard(Id) ->
	is_object_flag_set(Id, ?OBJECT_WIZARD).

-spec set_wizard_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_wizard_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_WIZARD, Value).

-spec is_programmer(Id::objid()) -> boolean() | 'E_INVARG'.
is_programmer(Id) ->
	is_object_flag_set(Id, ?OBJECT_PROG).

-spec set_programmer_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_programmer_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_PROG, Value).

-spec is_readable(Id::objid()) -> boolean() | 'E_INVARG'.
is_readable(Id) ->
	is_object_flag_set(Id, ?OBJECT_READ).

-spec set_read_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_read_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_READ, Value).

-spec is_writable(Id::objid()) -> boolean() | 'E_INVARG'.
is_writable(Id) ->
	is_object_flag_set(Id, ?OBJECT_WRITE).

-spec set_write_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_write_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_WRITE, Value).

-spec is_fertile(Id::objid()) -> boolean() | 'E_INVARG'.
is_fertile(Id) ->
	is_object_flag_set(Id, ?OBJECT_FERTILE).

-spec set_fertile_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_fertile_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_FERTILE, Value).

%%%============================================================================
%%% Adding and removing properties, manipulating property values.
%%%============================================================================

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
%% @doc Deletes property with Key from object with Id.
%%-----------------------------------------------------------------------------
delete_property(Id, Key) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> 
			case lists:keyfind(Key, 1, Obj#object.props) of
				false -> 'E_PROPNF';
				{_Key, {_V, _Info}} ->
					NewProps = proplists:delete(Key, Obj#object.props),
					ets:insert(?TABLE_OBJECTS, Obj#object{props = NewProps})
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
get_value(Id, wizard) -> is_wizard(Id);
get_value(Id, programmer) -> is_programmer(Id);
get_value(Id, r) -> is_readable(Id);
get_value(Id, w) -> is_writable(Id);
get_value(Id, f) -> is_fertile(Id);
get_value(Id, player) -> is_player(Id);
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
set_value(Id, wizard, Value) -> set_wizard_flag(Id, Value);
set_value(Id, programmer, Value) -> set_programmer_flag(Id, Value);
set_value(Id, r, Value) -> set_read_flag(Id, Value);
set_value(Id, w, Value) -> set_write_flag(Id, Value);
set_value(Id, f, Value) -> set_fertile_flag(Id, Value);
set_value(Id, player, Value) -> set_player_flag(Id, Value);
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

-spec is_object_flag_set(Id::objid(), Flag::integer()) -> 
	'E_INVARG' | boolean().
is_object_flag_set(Id, Flag) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> is_flag_set(Flag, Obj#object.flags)
	end.

-spec update_object_flag(Id::objid(), Flag::integer(), Value::boolean()) ->
	'E_INVARG' | true.
update_object_flag(Id, Flag, Value) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			NewFlags = update_flags(Flag, Value, Obj#object.flags),
			ets:insert(?TABLE_OBJECTS, Obj#object{flags = NewFlags})
	end.

-spec update_flags(Flag::integer(), Value::boolean(), Flags::integer()) -> 
	NewFlags::integer().
update_flags(Flag, Value, Flags) ->
	case Value of 
		true -> Flags bor Flag;
		_ -> Flags band (bnot Flag)
	end.

is_flag_set(Flag, Flags) ->
	Flags band Flag =:= Flag.

%% Used internally to keep track of object ids
-spec update_counter(Key::atom(), Incr::integer()) -> integer().
update_counter(Key, Incr) ->
	ets:update_counter(?TABLE_COUNTERS, Key, Incr).