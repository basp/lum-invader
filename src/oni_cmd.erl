%%%----------------------------------------------------------------------------
%%% @copyright 2013-2014 Bas Pennings [http://github.com/basp]
%%% @doc Command parser.
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
-module(oni_cmd).

-export([parse/2, 
         verb/1, 
         argstr/1, args/1, 
         dobjstr/1, dobj/1, 
         prepstr/1, 
         iobjstr/1, iobj/1]).

-type cmdspec() :: 
    {Verb::binary(), Dobjstr::binary(), Argstr::binary(), Args::[binary()]} |
    {Verb::binary(), Dobjstr::binary(),
     Prep::binary(), Iobjstr::binary(), Argstr::binary(), Args::[binary()]}.

-record(parsed_cmd, {verb = <<>>, argstr = <<>>, args = [], 
                     dobjstr = <<>>, dobj = nothing, 
                     prepstr = <<>>, iobjstr = <<>>, iobj = nothing}).

%%%============================================================================
%%% API
%%%============================================================================

%% First, some simple access functions. 

%% @doc Extracts the verb element from a parsed command.
-spec verb(#parsed_cmd{})                   -> binary().
verb(#parsed_cmd{verb = Verb})              -> Verb.

%% @doc Extracts the argstr element from a parsed command.
-spec argstr(#parsed_cmd{})                 -> binary().
argstr(#parsed_cmd{argstr = Argstr})        -> Argstr.

%% @doc Extracts the args element from a parsed command.
-spec args(#parsed_cmd{})                   -> [binary()].
args(#parsed_cmd{args = Args})              -> Args.

%% @doc Extracts the dobjstr element from a parsed command.
-spec dobjstr(#parsed_cmd{})                -> binary().
dobjstr(#parsed_cmd{dobjstr = Dobjstr})     -> Dobjstr.

%% @doc Extracts the dobj element from a parsed command.
-spec dobj(#parsed_cmd{})                   -> oni_db:objid().
dobj(#parsed_cmd{dobj = Dobj})              -> Dobj.

%% @doc Extracts the prepstr element from a parsed command.
-spec prepstr(#parsed_cmd{})                -> binary().
prepstr(#parsed_cmd{prepstr = Preopstr})    -> Preopstr.

%% @doc Extracts the iobjstr element from a parsed command.
-spec iobjstr(#parsed_cmd{})                -> binary().
iobjstr(#parsed_cmd{iobjstr = Iobjstr})     -> Iobjstr.

%% @doc Extracts the iobj element from a parsed command.
-spec iobj(#parsed_cmd{})                   -> oni_db:objid().
iobj(#parsed_cmd{iobj = Iobj})              -> Iobj.

%% @doc Parses a user command.
-spec parse(Data::binary(), User::oni_db:objid()) -> #parsed_cmd{}.
parse(Data, nothing) ->
    case parse_spec(Data) of
        {Verb, _Dobjstr, Argstr, Args} ->
            #parsed_cmd{verb = Verb, argstr = Argstr, args = Args};
        {Verb, _Dobjstr, _Prepstr, _Iobjstr, Argstr, Args} ->
            #parsed_cmd{verb = Verb, argstr = Argstr, args = Args}
    end;
parse(Data, User) ->
    case parse_spec(Data) of
        {Verb, Dobjstr, Argstr, Args} ->
            #parsed_cmd{verb = Verb, argstr = Argstr, args = Args,
                        dobjstr = Dobjstr, 
                        dobj = resolve_objstr(Dobjstr, User)};
        {Verb, Dobjstr, Prepstr, Iobjstr, Argstr, Args} ->
            #parsed_cmd{verb = Verb, argstr = Argstr, args = Args,
                        dobjstr = Dobjstr,
                        dobj = resolve_objstr(Dobjstr, User),
                        prepstr = Prepstr, 
                        iobjstr = Iobjstr,
                        iobj = resolve_objstr(Iobjstr, User)};
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec resolve_objstr(binary(), oni_db:objid() | [oni_db:objid()]) -> 
    oni_db:objid().
%% We're resolving on an empty list, nothing to find.
resolve_objstr(_Str, []) -> 
    nothing;
%% Check if we are indexing on a direct object or indirect object.
%% If we are, we are going to use list_i instead of list.
resolve_objstr(Str, List) when is_list(List) ->
    case Str of
        %% Optimized cases for index 1-9
        <<"1." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 1);
        <<"2." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 2);
        <<"3." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 3);
        <<"4." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 4);
        <<"5." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 5);
        <<"6." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 6);
        <<"7." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 7);
        <<"8." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 8);
        <<"9." , Rest>>     -> oni_match:list_i(match_object(Rest), List, 9);
        %% We can add more optimized cases here if needed
        %% <<"10.", Rest>>     -> ...
        %%
        <<C, C, $., Rest>>  ->
            %% TODO: This needs to be more safe.
            Index = binary_to_integer(<<C, C>>),
            oni_match:list_i(match_object(Rest), List, Index);
        Str ->
            oni_match:list(match_object(Str), List)
    end;
%% We are resolving an empty string, never gonna find a result.
resolve_objstr(<<>>, _User) -> 
    nothing;
%% We are resolving an object id like '#123'.
%% At this point we are going to assume the user is sane and is not passing
%% us something like '#foo' which cannot be parsed safely into an 'integer()'.
resolve_objstr(<<$#, Rest>>, _User) -> 
    %% TODO: This needs to be more safe.
    binary_to_integer(Rest);
%% This is easy, we are referring to the user so we can just return that.
resolve_objstr(<<"me">>, User) -> 
    User;
%% This is also easy, we need to get the user's current location. 
%% We can easily obtain that and return it.
resolve_objstr(<<"here">>, User) -> 
    oni_db:location(User);
%% We are trying to get to something else so we need to do some more
%% involved matching.
%%
%% @TODO This is not complete, we need to match more completely.
resolve_objstr(Str, User) ->
    resolve_objstr(Str, oni_db:contents(User)).

%% Utility to match on an object name and/or it's aliases.
match_object(Str) -> 
    fun(Id) -> 
        Names = lists:filter(
            fun(X) -> is_binary(X) end,
            [oni_db:name(Id) | oni_db:aliases(Id)]),
        lists:any(fun(X) -> oni_bstr:starts_with(Str, X) end, Names)
    end.

%% This parses to a raw command spec tuple that is used internally.
-spec parse_spec(binary()) -> cmdspec().
parse_spec(Data) ->
    %% The first token is assumed to be the verb.
    token(Data, fun(Rest, Verb) -> 
        whitespace(Rest, fun(Rest2) ->
            command(Rest2, {Verb})
        end)
    end).

%% This will perform the bulk of the command parsing, the state is 
%% determined by the composition of the state tuple that is passed around.
-spec command(binary(), {binary()}) -> cmdspec().
%% Step 1: 
%% Before entering this step we assume that the verb has been parsed and
%% that any whitespace is stripped. We are now about the parse the next token.
%% The next token should always be a direct object (or nothing). Note that
%% the data we get in this function clause is returned as the final argstr
%% value to the caller.
command(Data, {Verb}) ->
    words_until_preposition(Data, fun(Rest, Dobj) -> 
        command(Rest, _State = {Verb, Dobj, Data})
    end);
%% Step 2 (sometimes final):
%% Look for a preposition and continue, if we can't find a preposition at this 
%% point then we are done. In that case, we just consider everything past the 
%% verb as part of the direct object and return early. If we find a 
%% preposition, we continue and try to parse the indirect object in step 3.
command(Data, {Verb, Dobj, Argstr}) ->
    case preposition(Data) of
        false -> 
            Dobjstr = oni_bstr:join(Dobj),
            Args = Dobj,
            {Verb, Dobjstr, oni_bstr:trim(Argstr), Args};
        {Prep, Rest} ->
            whitespace(Rest, fun(Rest2) -> 
                    command(Rest2, {Verb, Dobj, Prep, Argstr}) 
            end)
    end;
%% Step 3 (optional and always final):
%% Found a preposition, finish with the indirect object (if there is one).
%% Everything we find from now will be part of the indirect object until we
%% run out of data to parse.
command(Data, {Verb, Dobj, Prep, Argstr}) -> 
    words(Data, fun(_Rest, Iobj) ->
        Dobjstr = oni_bstr:join(Dobj),
        Iobjstr = oni_bstr:join(Iobj),
        Args = lists:concat([Dobj, [Prep], Iobj]),
        {Verb, Dobjstr, Prep, Iobjstr, oni_bstr:trim(Argstr), Args}
    end).

-spec words(binary(), fun()) -> any().
words(Data, Fun) ->
    words(Data, Fun, []).

-spec words(binary(), fun(), [binary()]) -> any().
words(<<>>, Fun, Acc) ->
    Fun(<<>>, lists:reverse(Acc));
words(Data, Fun, Acc) ->
        word(Data, fun(Rest, Word) ->
            whitespace(Rest, fun(Rest2) ->
                words(Rest2, Fun, [Word|Acc])
            end)
        end).

-spec words_until_preposition(binary(), fun()) -> any().
words_until_preposition(Data, Fun) ->
    words_until_preposition(Data, Fun, []).

-spec words_until_preposition(binary(), fun(), [binary()]) -> any().
words_until_preposition(<<>>, Fun, Acc) ->
    Fun(<<>>, lists:reverse(Acc));
words_until_preposition(Data, Fun, Acc) ->
    case preposition(Data) of
        {_, _} -> Fun(Data, lists:reverse(Acc));
        false -> 
            word(Data, fun(Rest, Word) ->
                whitespace(Rest, fun(Rest2) ->
                    words_until_preposition(Rest2, Fun, [Word|Acc])
                end)
            end)
    end.

-spec whitespace(binary(), fun()) -> any().
whitespace(<<C, Rest/binary>>, Fun) 
    when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
        whitespace(Rest, Fun);
whitespace(Data, Fun) ->
    Fun(Data).

-spec word(binary(), fun()) -> any().
word(Data = <<$", _/binary>>, Fun) ->
    quoted_string(Data, Fun);
word(Data, Fun) ->
    token(Data,
        fun(_Rest, <<>>) -> {error, badarg};
           (Rest, Token) -> Fun(Rest, Token)
        end).

-spec token(binary(), fun()) -> any().
token(Data, Fun) ->
    token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) -> 
    Fun(<<>>, Acc);
token(Data = <<C, _Rest/binary>>, Fun, _Case, Acc) 
    when C =:= $\s; C =:= $\t; C =:= $\r; C =:= $\n ->
        Fun(Data, Acc);
token(<<$", Rest/binary>>, Fun, Case, Acc) ->
    token(Rest, Fun, Case, Acc);
token(<<C, Rest/binary>>, Fun, Case = ci, Acc) ->
    token(Rest, Fun, Case, <<Acc/binary, C>>);
token(<<C, Rest/binary>>, Fun, Case = cs, Acc) ->
    token(Rest, Fun, Case, <<Acc/binary, C>>).

-spec quoted_string(binary(), fun()) -> any().
quoted_string(<<$", Rest/binary>>, Fun) ->
    quoted_string(Rest, Fun, <<>>).

-spec quoted_string(binary(), fun(), binary()) -> any().
quoted_string(<<$", Rest/binary>>, Fun, <<>>) ->
    quoted_string(Rest, Fun, <<>>);
quoted_string(<<$", $\s, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<$", $\t, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<$", $\r, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<$", $\n, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<>>, Fun, Acc) ->
    Fun(<<>>, Acc);
quoted_string(<<C, Rest/binary>>, Fun, Acc) ->
    quoted_string(Rest, Fun, <<Acc/binary, C>>).

%% We need to be able to distinguish between "normal" words and
%% prepositions in order to split the command into dobj and iobj parts.
-spec preposition(Data::binary()) -> 
    {Prep::binary(), Rest::binary()} | false.
preposition(Data) ->
    case Data of
        <<"with ", Rest/binary>>        -> {<<"with">>, Rest};
        <<"at ", Rest/binary>>          -> {<<"at">>, Rest};
        <<"to ", Rest/binary>>          -> {<<"to">>, Rest};
        <<"in front of ", Rest/binary>> -> {<<"in front of">>, Rest};
        <<"in ", Rest/binary>>          -> {<<"in">>, Rest};
        <<"from ", Rest/binary>>        -> {<<"from">>, Rest};
        <<"inside ", Rest/binary>>      -> {<<"inside">>, Rest};
        <<"into ", Rest/binary>>        -> {<<"into">>, Rest};
        <<"on top of ", Rest/binary>>   -> {<<"on top of">>, Rest};
        <<"on ", Rest/binary>>          -> {<<"on">>, Rest};
        <<"onto ", Rest/binary>>        -> {<<"onto">>, Rest};
        <<"upon ", Rest/binary>>        -> {<<"upon">>, Rest};
        <<"using ", Rest/binary>>       -> {<<"using">>, Rest};
        <<"out of ", Rest/binary>>      -> {<<"out of">>, Rest};
        <<"from inside ", Rest/binary>> -> {<<"from inside">>, Rest};
        <<"over ", Rest/binary>>        -> {<<"over">>, Rest};
        <<"through ", Rest/binary>>     -> {<<"through">>, Rest};
        <<"under ", Rest/binary>>       -> {<<"under">>, Rest};
        <<"underneath ", Rest/binary>>  -> {<<"underneath">>, Rest};
        <<"behind ", Rest/binary>>      -> {<<"behind">>, Rest};
        <<"for ", Rest/binary>>         -> {<<"for">>, Rest};
        <<"about ", Rest/binary>>       -> {<<"about">>, Rest};
        <<"is ", Rest/binary>>          -> {<<"is">>, Rest};
        <<"as ", Rest/binary>>          -> {<<"as">>, Rest};
        <<"off ", Rest/binary>>         -> {<<"off">>, Rest};
        <<"off of ", Rest/binary>>      -> {<<"off of">>, Rest};
        _ -> false
    end.