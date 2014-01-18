%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Implements the callbacks for an accepted socket connection.
%%%
%%% This module does some lifting by doing some first chance checks on the
%%% binary data. For now, the basic behaviour is to parse the command and return
%%% the pretty (not really) command spec in text form. 
%%%
%%% There are some special cases though which are available if you are a wizard.
%%% You can execute any Erlang expression list by prefixing your command with a
%%% semicolon (";") and store values in your session bindings by assigning them 
%%% to a name(e.g. ";NewPlayer = oni_db:create(nothing)."). You can reset your
%%% bindings with the special command "@reset". 
%%%
%%% Every player can execute the "@quit" command. This will gracefully
%%% disconnect.
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
-module(oni_sockserv_serv).
-behaviour(gen_server).

-record(state, {listener, 
                next, 
                bindings = [],
                player = nothing}).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(MSG_CONNECT, <<"$bold;$fg_magenta;Oni $fg_yellow;[$fg_cyan;lum invader$fg_yellow;]$reset;">>).
-define(INVALID_CMD, <<"That doesn't seem right.">>).

%%%============================================================================    
%%% API 
%%%============================================================================

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%%%============================================================================    
%%% gen_server callbacks
%%%============================================================================

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{listener = Socket}}.

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, S = #state{listener = ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    oni_sockserv_sup:start_socket(),
    oni_event:connected(AcceptSocket),
    %% oni:notify(AcceptSocket, oni_ansi:style(?MSG_CONNECT)),0
    {ok, MOTD} = file:read_file("MOTD"),
    oni:notify(AcceptSocket, oni_ansi:style(MOTD)),
    {noreply, S#state{next = capabilities}}.
    
handle_info({tcp, _Socket, <<"@quit", _/binary>>}, S) ->
    %% Handle @quit early so we don't depend on too much matching state.
    {stop, normal, S};
handle_info({tcp, Socket, _Data}, S = #state{next = capabilities}) ->
    %% We should parse capability info right here but 
    %% we'll just reset the socket for now.
    oni:notify(Socket, <<"Please login to an existing player.">>),
    {noreply, S#state{next = login}};
handle_info({tcp, Socket, Data}, S = #state{next = login}) 
        when is_binary(Data) ->
    Cmd = oni_cmd:parse(Data, nothing),
    case oni:do_login(Socket, oni_cmd:verbstr(Cmd), oni_cmd:args(Cmd)) of
        nothing -> {noreply, S};
        Player -> 
            oni_who:insert_connection(Player, Socket),
            oni_event:player_connected(Socket, Player),
            case oni_aq_sup:start_queue(Player) of
                {already_started, Pid} -> oni_aq_serv:next(Pid);
                _ -> ok
            end,
            {noreply, S#state{next = connected, player = Player}}
    end;
handle_info({tcp, _Socket, <<"@reset", _/binary>>}, 
             S = #state{next = connected, player = Player}) ->
    NewBindings = handle_reset(Player, S#state.bindings),
    {noreply, S#state{bindings = NewBindings}};
handle_info({tcp, _Socket, <<";", Data/binary>>},
             S = #state{next = connected, player = Player, 
                        bindings = Bindings}) ->
    NewBindings = handle_eval(Player, Data, Bindings),
    {noreply, S#state{bindings = NewBindings}};
handle_info({tcp, _Socket, <<"say ", Data/binary>>},
             S = #state{next = connected, player = Player}) ->
    oni:say(Player, oni_bstr:trim(Data)),
    {noreply, S};
handle_info({tcp, _Socket, <<"'", Data/binary>>}, 
             S = #state{next = connected, player = Player}) ->
    oni:say(Player, oni_bstr:trim(Data)),
    {noreply, S};
handle_info({tcp, _Socket, <<"emote ", Data/binary>>},
             S = #state{next = connected, player = Player}) ->
    oni:emote(Player, oni_bstr:trim(Data)),
    {noreply, S};
handle_info({tcp, _Socket, <<":", Data/binary>>},
             S = #state{next = connected, player = Player}) ->
    oni:emote(Player, oni_bstr:trim(Data)),
    {noreply, S};
handle_info({tcp, Socket, Data}, 
             S = #state{next = connected, player = Player}) ->
    Command = oni_cmd:parse(Data, Player),
    case oni_pack:cmd(Command, Player) of
        {error, Command} -> 
            handle_pack_error(Socket, Command);
        Pack -> 
            %% oni_aq_sup:queue(Player, Pack)
            oni_rt_serv:exec(Pack)
    end,
    {noreply, S};
handle_info({tcp_closed, Socket}, 
             S = #state{next = connected, player = Player}) ->
    oni_who:delete_connection(Player),
    oni_event:player_disconnected(Socket, Player),
    {stop, normal, S};
handle_info({tcp_closed, Socket}, S) ->
    oni_event:disconnected(Socket),
    {stop, normal, S}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%============================================================================    
%%% Internal functions
%%%============================================================================

handle_pack_error(Socket, Command) ->
    case oni_cmd:verbstr(Command) of
        <<>>    -> inet:setopts(Socket, [{active, once}, {mode, binary}]);
        _Else   -> oni:notify(Socket, "[huh?] ~p", [Command])
    end.

handle_eval(Player, Data, Bindings) ->
    case oni_db:is_wizard(Player) of
        true -> 
            {Str, NewBindings} = oni:eval_to_str(Data, Bindings),
            oni:notify(Player, Str),
            NewBindings;
        false -> 
            oni:notify(Player, ?INVALID_CMD),
            Bindings
    end.

handle_reset(Player, Bindings) ->
    case oni_db:is_wizard(Player) of
        true -> 
            oni:notify(Player, <<"Environment reset.">>), 
            [];
        false -> 
            oni:notify(Player, ?INVALID_CMD), 
            Bindings
    end.