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

-export([parse/2]).

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
                        iobj = resolve_objstr(Iobjstr, User)}
    end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

-spec resolve_objstr(binary(), oni_db:objid() | [oni_db:objid()]) -> 
    oni_db:objid().
resolve_objstr(_Str, []) -> nothing;
resolve_objstr(Str, [H|T]) ->
    oni_match:list(match_object(Str), [H|T]);
resolve_objstr(<<>>, _User) -> nothing;
resolve_objstr(<<$#, Rest>>, _User) -> binary_to_integer(Rest);
resolve_objstr(<<"me">>, User) -> User;
resolve_objstr(<<"here">>, User) -> oni_db:location(User);
resolve_objstr(Str, User) ->
    resolve_objstr(Str, oni_db:contents(User)).

match_object(Str) -> 
    fun(Id) -> 
        case oni_db:name(Id) of
            nothing -> false;
            Name -> oni_bstr:starts_with(Str, Name)
        end
    end.

-spec parse_spec(binary()) -> cmdspec().
parse_spec(Data) ->
    %% The first token is assumed to be the verb.
    token(Data, fun(Rest, Verb) -> 
        whitespace(Rest, fun(Rest2) ->
            command(Rest2, {Verb})
        end)
    end).

-spec command(binary(), {binary()}) -> cmdspec().
command(Data, {Verb}) ->
    %% Parse the dobj and also pass along everything else (trimmed) 
    %% as our argstr.
    words_until_preposition(Data, fun(Rest, Dobj) -> 
        command(Rest, {Verb, Dobj, oni_bstr:trim(Data)})
    end);
command(Data, {Verb, Dobj, Argstr}) ->
    %% Look for a preposition and continue, if we can't
    %% find a preposition at this point then we are done.
    case preposition(Data) of
        false -> 
            Dobjstr = oni_bstr:join(Dobj),
            Args = Dobj,
            {Verb, Dobjstr, Argstr, Args};
        {Prep, Rest} ->
            whitespace(Rest, fun(Rest2) -> 
                    command(Rest2, {Verb, Dobj, Prep, Argstr}) 
            end)
    end;
command(Data, {Verb, Dobj, Prep, Argstr}) -> 
    %% Found a preposition, finish with the iobj part.
    words(Data, fun(_Rest, Iobj) ->
        Dobjstr = oni_bstr:join(Dobj),
        Iobjstr = oni_bstr:join(Iobj),
        Args = lists:concat([Dobj, [Prep], Iobj]),
        {Verb, Dobjstr, Prep, Iobjstr, Argstr, Args}
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
quoted_string(<<>>, _Fun, _Acc) ->
    {error, badarg};
quoted_string(<<$", $\s, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<$", $\t, Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<$", Rest/binary>>, Fun, Acc) ->
    quoted_string(Rest, Fun, Acc);
quoted_string(<<$\\, C, Rest/binary>>, Fun, Acc) ->
    quoted_string(Rest, Fun, <<Acc/binary, C>>);
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