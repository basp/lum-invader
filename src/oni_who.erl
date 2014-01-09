%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Inspecting and manipulating players and active connections.
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
-module(oni_who).

-export([init/0, insert_connection/2, delete_connection/1, get_connection/1,
         get_player/1, list/0]).

-record(conn, {id, socket}).

-define(TABLE_CONNECTIONS, oni_connections).

%%%============================================================================
%%% API
%%%============================================================================

%% @doc Initializes the connections table.
%%
%% This table is used to connect object ids and sockets.
-spec init() -> true.
init() ->
    Args = [set, {keypos, #conn.id}, named_table, public],
    ets:new(?TABLE_CONNECTIONS, Args).

%% @doc Inserts a connection into the table.
%%
%% Any existing connection with the same object id will be blissfully 
%% overwritten.
-spec insert_connection(Id::oni_db:objid(), Socket::gen_tcp:socket()) -> true.
insert_connection(Id, Socket) ->
    ets:insert(?TABLE_CONNECTIONS, #conn{id = Id, socket = Socket}).

%% @doc Deletes the connection associated with given object id.
%%
%% This will hapilly return with true if there are no connections for 
%% given id.
-spec delete_connection(Id::oni_db:objid()) -> true.
delete_connection(Id) ->
    ets:delete(?TABLE_CONNECTIONS, Id).

%% @doc Returns the connection associated with given object id.
-spec get_connection(Id::oni_db:objid()) -> gen_tcp:socket().
get_connection(Id) ->
    case ets:lookup(?TABLE_CONNECTIONS, Id) of
        [] -> nothing;
        [{conn, _Id, Socket}] -> Socket
    end.

%% @doc Returns the object id corresponding to the given socket.
-spec get_player(Socket::gen_tcp:socket()) -> oni_db:objid().
get_player(Socket) ->
    M = [{#conn{id = '$1', socket = Socket}, [], ['$1']}],
    case ets:select(?TABLE_CONNECTIONS, M) of
        [] -> nothing;
        [Player] -> Player
    end.

%% @doc Returns a list of all active connection associations.
-spec list() -> [{Id::oni_db:objid(), Socket::gen_tcp:socket()}].
list() ->
    M = [{#conn{id = '$1', socket = '$2'}, [], [{{'$1', '$2'}}]}],
    ets:select(?TABLE_CONNECTIONS, M).