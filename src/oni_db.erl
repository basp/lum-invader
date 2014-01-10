%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Manipulating the world.
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
-module(oni_db).

-include_lib("stdlib/include/qlc.hrl").

-export([init/0, create_wiz/2, 
		 create/1, valid/1, recycle/1, max_object/0, 
		 name/1, rename/2, 
		 aliases/1, set_aliases/2,
		 chparent/2, parent/1, children/1, 	 
		 move/2, location/1, contents/1, 
		 players/0, is_player/1, set_player_flag/2, 
		 is_wizard/1, set_wizard_flag/2, 
         is_programmer/1, set_programmer_flag/2, 
         is_readable/1, set_read_flag/2, 
         is_writable/1, set_write_flag/2, 
         is_fertile/1, set_fertile_flag/2, 
         properties/1, add_property/3, delete_property/2, 
         get_value/2, set_value/3,
         verbs/1, add_verb/3, delete_verb/2,
         verb_args/2, set_verb_args/3,
         verb_info/2, set_verb_info/3,
         verb_code/2, set_verb_code/3]).

-record(object, {id, 
                 parent = nothing, 
                 name = <<"">>,
                 aliases = [],
                 location = nothing, 
                 verbs = [], 
                 props = [], 
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

%% Maybe this should be an opaque type wrapping the integer() instead...
-type objid() :: integer().

-type verb_args()::{(this | none | any), (none | any), (this | none | any)}.

-type verb_info()::{Owner::objid(), Names::[binary()]}.

-type verb_code()::{Module::atom(), Function::atom()}.

%% -type verb_spec()::{verb_info(), verb_args()}.

-export_type([objid/0]).

%%%============================================================================
%%% Initializing the world database
%%%============================================================================

%% @doc Initializes the object table.
init() ->
	%% Stores all the objects that we know about
	ets:new(?TABLE_OBJECTS, [set, {keypos, #object.id}, named_table, public]),
	ets:insert(?TABLE_OBJECTS, #object{id = 0}),
	%% Keeps track of max object id and any other counters
	ets:new(?TABLE_COUNTERS, [set, named_table, public]),
	ets:insert(?TABLE_COUNTERS, {max_id, 0}).

%% @doc Utility method to create initial player (wizard). 
%%
%% This function is here for development and meant to be called from the shell.
%% It's useful to bootstrap the first connection after sstarting the application 
%% but that is really the only way it should be used.
%%
%% Also, it's useful as an example of the sequence of expressions that have 
%% to be evaluated in order to set the wizard state for an object.
%% @end
-spec create_wiz(Parent::objid(), Name::binary()) -> objid().
create_wiz(Parent, Name) ->
    Id = oni_db:create(Parent),
    oni_db:set_player_flag(Id, true),
    oni_db:set_wizard_flag(Id, true),
    oni_db:rename(Id, Name).

%%%============================================================================
%%% Core object manipulation.
%%%============================================================================

%% @doc Creates a new object and returns the id of the newly created object.
-spec create(Parent::objid()) -> objid().
create(Parent) ->
	Id = update_counter(max_id, 1),
	ets:insert(?TABLE_OBJECTS, #object{id = Id, parent = Parent}),
	Id.

%% @doc Determines whether the given object id belongs to an existing object.
-spec valid(Id::objid()) -> boolean().
valid(Id) when Id < 0 -> 
	false;
valid(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> false;
		_ -> true
	end.

%% @doc Returns the name of object with id.
-spec name(Id::objid()) -> binary().
name(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.name
	end.

%% @doc Sets the name of object with id.
-spec rename(Id::objid(), Name::binary()) -> 'E_INVARG' | true.
rename(Id, Name) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			ets:insert(?TABLE_OBJECTS, Obj#object{name = Name})
	end.

%% @doc Returns the aliases of given object.
aliases(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.aliases
	end.

%% @doc Sets the aliases of given object to given list.
set_aliases(Id, Value) when is_list(Value) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			ets:insert(?TABLE_OBJECTS, Obj#object{aliases = Value})
	end.

%% @doc Changes the parent of the object with specified id.
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

%% @doc Returns the parent of the object with specified id.
-spec parent(Id::objid()) -> objid().
parent(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.parent
	end.

%% @doc Returns the objects that have their parent set to specified id.
-spec children(Id::objid()) -> [objid()] | 'E_INVARG'.
children(Id) ->
	case valid(Id) of 
		false -> ' E_INVARG';
		true ->
			M = [{#object{id = '$1', parent = Id, _='_'}, [], ['$1']}],
			ets:select(?TABLE_OBJECTS, M)
	end.

%% @doc Destroys the object with specified id.
%%
%% The contents of the destroyed object will have their location set to
%% nothing after this operation completes. 
%%
%% Note that we explicitly don't manipulate the max_id counter here - we want 
%% old ids to point to invalid objects and not to have them silently replaced
%% by a completely new object with an old id.
%% @end
-spec recycle(Id::objid()) -> true | 'E_INVARG'.
recycle(Id) ->
	case valid(Id) of
		false -> 'E_INVARG';
		true -> 
			lists:foreach(fun(X) -> move(X, nothing) end, contents(Id)),
			ets:delete(?TABLE_OBJECTS, Id)
	end.

%% @doc Returns the largest object id assigned to an object.
-spec max_object() -> objid().
max_object() ->
	case ets:lookup(?TABLE_COUNTERS, max_id) of
		[] -> nothing;
		[{max_id, Id}] -> Id
	end.

%% @doc Moves what to where.
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

%% @doc Returns the location of the object with specified id.
-spec location(Id::objid()) -> objid().
location(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> Obj#object.location
	end.

%% @doc Returns the objects that have their location set to specified id.
-spec contents(Id::objid()) ->	[objid()] | 'E_INVARG'.
contents(Id) ->
	case valid(Id) of
		false -> 'E_INVARG';
		true ->
			M = [{#object{id = '$1', location = Id, _='_'}, [], ['$1']}],
			ets:select(?TABLE_OBJECTS, M)
	end.

%% @doc Returns a list of all objects that have the player flag set.
-spec players() -> [objid()].
players() ->
	Q = qlc:q([O#object.id || 
		       O <- ets:table(?TABLE_OBJECTS), is_player(O#object.id)]),
	qlc:e(Q).

%%%============================================================================
%%% Manipulating object flags.
%%%============================================================================

%% @doc Determines whether the given object has its player flag set.
-spec is_player(Id::objid()) -> boolean() | 'E_INVARG'.
is_player(Id) -> 
	is_object_flag_set(Id, ?OBJECT_PLAYER).

%% @doc Sets the player flag on the given object.
-spec set_player_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_player_flag(Id, Value) -> 
	update_object_flag(Id, ?OBJECT_PLAYER, Value).

%% @doc Determines whether the given object has its wizard flag set.
-spec is_wizard(Id::objid()) -> boolean() | 'E_INVARG'.
is_wizard(Id) ->
	is_object_flag_set(Id, ?OBJECT_WIZARD).

%% @doc Sets the wizard flag on the given object.
-spec set_wizard_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_wizard_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_WIZARD, Value).

%% @doc Determines whether the given object has its programmer flag set.
-spec is_programmer(Id::objid()) -> boolean() | 'E_INVARG'.
is_programmer(Id) ->
	is_object_flag_set(Id, ?OBJECT_PROG).

%% @doc Sets the programmer flag on the given object.
-spec set_programmer_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_programmer_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_PROG, Value).

%% @doc Determines whether the given object has its read flag set.
-spec is_readable(Id::objid()) -> boolean() | 'E_INVARG'.
is_readable(Id) ->
	is_object_flag_set(Id, ?OBJECT_READ).

%% @doc Sets the read flag on the given object.
-spec set_read_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_read_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_READ, Value).

%% @doc Determines whether the given object has its write flag set.
-spec is_writable(Id::objid()) -> boolean() | 'E_INVARG'.
is_writable(Id) ->
	is_object_flag_set(Id, ?OBJECT_WRITE).

%% @doc Sets the write flag on the given object.
-spec set_write_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_write_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_WRITE, Value).

%% @doc Determines whether the given object has its fertile flag set.
-spec is_fertile(Id::objid()) -> boolean() | 'E_INVARG'.
is_fertile(Id) ->
	is_object_flag_set(Id, ?OBJECT_FERTILE).

%% @doc Sets the fertile flag on the given object.
-spec set_fertile_flag(Id::objid(), Value::boolean()) -> true | 'E_INVARG'.
set_fertile_flag(Id, Value) ->
	update_object_flag(Id, ?OBJECT_FERTILE, Value).

%%%============================================================================
%%% Adding and removing properties, manipulating property values.
%%%============================================================================

%% @doc Returns a list of all (non-builtin) properties.
-spec properties(Id::objid()) -> [atom()] | 'E_INVARG'.
properties(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> proplists:get_keys(Obj#object.props)
	end.

%% @doc Adds a property with Key and Value to object with Id.
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

%% @doc Deletes property with Key from object with Id.
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

%% @doc Returns the value of the object's property with specified key.
-spec get_value(Id::objid(), Key::atom()) ->
	'E_INVARG' | 'E_PROPNF' | any().
get_value(Id, id) -> Id;
get_value(Id, parent) -> parent(Id);
get_value(Id, location) -> location(Id);
get_value(Id, contents) -> contents(Id);
get_value(Id, children) -> children(Id);
get_value(Id, name) -> name(Id);
get_value(Id, aliases) -> aliases(Id);
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

%% @doc Sets the value of the object's property with specified key.
%%
%% We'll return 'E_INVARG' if someone tries to set readonly properties. These
%% properties should be manipulated with API functions in this module instead.
%% @end
-spec set_value(Id::objid(), Key::atom(), Value::any()) -> 
	'E_INVARG' | 'E_PROPNF' | any().
set_value(_Id, id, _Value) -> 'E_INVARG';
set_value(_Id, parent, _Value) -> 'E_INVARG';
set_value(_Id, location, _Value) -> 'E_INVARG';
set_value(_Id, contents, _Value) -> 'E_INVARG';
set_value(_Id, children, _Value) -> 'E_INVARG';
set_value(Id, name, Value) -> rename(Id, Value);
set_value(Id, aliases, Value) -> set_aliases(Id, Value);
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
%%% Operations on verbs
%%%============================================================================

-spec verbs(Id::objid()) -> [binary()].
verbs(Id) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> 
			F = fun({{_Owner, Names}, _Args, _Code}) -> Names end,
			lists:concat(lists:map(F, Obj#object.verbs))
	end.

-spec add_verb(Id::objid(), Info::verb_info(), Args::verb_args()) -> 
	true | 'E_INVARG'.
add_verb(Id, Info = {_Owner, [_|_]}, Args = {_Dobj, _Prep, _Iobj}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			NewVerbs = [{Info, Args, none}|Obj#object.verbs],
			ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
	end.	

-spec delete_verb(Id::objid(), Verb::binary() | integer()) -> 
	true | 'E_INVARG' | 'E_VERBNF'.
delete_verb(Id, 1) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = []}] -> 'E_VERBNF';
		[Obj = #object{verbs = [_|T]}] -> 
			ets:insert(?TABLE_OBJECTS, Obj#object{verbs = T})
	end;
delete_verb(Id, N) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj = #object{verbs = Verbs}] ->
			try lists:split(N - 1, Verbs) of
				{L1, [_|L2]} -> 
					NewVerbs = lists:concat([L1, L2]),
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs});
				_ -> 'E_VERBNF'
			catch
				_:_ -> 'E_VERBNF'
			end
	end;
delete_verb(Id, Verb) when is_binary(Verb) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[_|T] -> ets:insert(?TABLE_OBJECTS, Obj#object{verbs = T})
			end
	end.

-spec verb_info(Id::objid(), Verb::binary() | integer()) -> 
	verb_info() | 'E_INVARG' | 'E_VERBNF'.
verb_info(Id, 1) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = [{X, _, _}|_]}] -> X;
		_ -> 'E_VERBNF'
	end;
verb_info(Id, N) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = Verbs}] ->
			try lists:nth(N, Verbs) of
				{X, _, _} -> X
			catch 
				_:_ -> 'E_VERBNF'
			end
	end;
verb_info(Id, Verb) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[{X, _, _}|_] -> X
			end
	end.

-spec set_verb_info(Id::objid(), Verb::binary() | integer(), 
					Info::verb_info()) -> true | 'E_INVARG' | 'E_VERBNF'.
set_verb_info(Id, 1, Info = {_Owner, [_|_]}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = []}] -> 'E_VERBNF';
		[Obj = #object{verbs = [{_, Args, Code}|T]}] ->
			NewVerbs = [{Info, Args, Code}|T],
			etl:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
	end;
set_verb_info(Id, N, Info = {_Owner, [_|_]}) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj = #object{verbs = Verbs}] ->
			try lists:split(N - 1, Verbs) of
				{L1, [{_, Args, Code}|L2]} ->
					NewVerbs = lists:concat([L1, [{Info, Args, Code}|L2]]),
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
			catch 
				_:_ -> 'E_VERBNF'
			end
	end;
set_verb_info(Id, Verb, Info = {_Owner, [_|_]}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[{_, Args, Code}|Rest] ->
					NewVerbs = [{Info, Args, Code}|Rest],
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
			end
	end.

-spec verb_args(Id::objid(), Verb::binary() | integer()) -> 
	verb_args() | 'E_INVARG' | 'E_VERBNF'.
verb_args(Id, 1) -> 
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = [{_,X,_}|_]}] -> X;
		_ -> 'E_VERBNF'
	end;
verb_args(Id, N) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = Verbs}] ->
			try lists:nth(N, Verbs) of
				{_, X, _} -> X
			catch 
				_:_ -> 'E_VERBNF'
			end
	end;
verb_args(Id, Verb) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[{_, Args, _}|_] -> Args
			end
	end.

-spec set_verb_args(Id::objid(), Verb::binary() | integer(),
					Args::verb_args()) -> true | 'E_INVARG' | 'E_VERBNF'.
set_verb_args(Id, 1, Args = {_Dobj, _Prep, _Iobj}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj = #object{verbs = [{Info, _, Code}|T]}] ->
			NewVerbs = [{Info, Args, Code}|T],
			ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
	end;
set_verb_args(Id, N, Args ={_Dobj, _Prep, _Iobj}) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj = #object{verbs = Verbs}] -> 
			try lists:split(N - 1, Verbs) of
				{L1, [{Info, _, Code}|L2]} ->
					NewVerbs = lists:concat([L1, [{Info, Args, Code}|L2]]),
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
			catch
				_:_ -> 'E_VERBNF'
			end
	end;
set_verb_args(Id, Verb, Args = {_Dobj, _Prep, _Iobj}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] -> 
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[{Info, _, Code}|Rest] ->
					NewVerbs = [{Info, Args, Code}|Rest],
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
			end
	end.

-spec verb_code(Id::objid(), Verb::binary() | integer()) -> 
	verb_code() | 'E_INVARG' | 'E_VERBNF'.
verb_code(Id, 1) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = [{_, _, X}|_]}] -> X;
		_ -> 'E_VERBNF'
	end;
verb_code(Id, N) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[#object{verbs = Verbs}] ->
			try lists:nth(N, Verbs) of
				{_, _, X} -> X
			catch
				_:_ -> 'E_VERBNF'
			end
	end;
verb_code(Id, Verb) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[{_, _, X}|_] -> X
			end
	end.

-spec set_verb_code(Id::objid(), Verb::binary() | integer(),
					Code::verb_code()) -> true | 'E_INVARG' | 'E_VERBNF'.
set_verb_code(Id, 1, Code = {_Module, _Function}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj = #object{verbs = [{Info, Args, _}|T]}] ->
			NewVerbs = [{Info, Args, Code}|T],
			ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
	end;
set_verb_code(Id, N, Code = {_Module, _Function}) when is_integer(N), N > 1 ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj = #object{verbs = Verbs}] ->
			try lists:split(N, Verbs) of
				{L1, [{Info, Args, _}|L2]} ->
					NewVerbs = lists:concat([L1, [{Info, Args, Code}|L2]]),
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
			catch
				_:_ -> 'E_VERBNF'
			end
	end;
set_verb_code(Id, Verb, Code = {_Module, _Function}) ->
	case ets:lookup(?TABLE_OBJECTS, Id) of
		[] -> 'E_INVARG';
		[Obj] ->
			case find_verbs(Obj#object.verbs, Verb) of
				[] -> 'E_VERBNF';
				[{Info, Args, _}|Rest] ->
					NewVerbs = [{Info, Args, Code}|Rest],
					ets:insert(?TABLE_OBJECTS, Obj#object{verbs = NewVerbs})
			end
	end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

find_verbs(Verbs, Str) ->
	F = fun({{_Owner, Names}, _Args, _Code}) ->
		lists:any(fun(X) -> X =:= Str end, Names)
	end,
	lists:filter(F, Verbs).

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