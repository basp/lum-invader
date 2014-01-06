%%%----------------------------------------------------------------------------
%%% @author Bas Pennings [http://github.com/basp]
%%% @copyright 2013-2014
%%% @end
%%%----------------------------------------------------------------------------
-module(oni_cmd).

-export([parse/1]).

%%----------------------------------------------------------------------------
%% NOTE:
%%   verb        a string, the first word of the command
%%   argstr      a string, everything after the first word of the command
%%   args        a list of strings, the words in `argstr'
%%   dobjstr     a string, the direct object string found during parsing
%%   prepstr     a string, the prepositional phrase found during parsing
%%   iobjstr     a string, the indirect object string
%%----------------------------------------------------------------------------
-type cmdspec() :: 
    {Verb::binary(), Dobjstr::binary(), Argstr::binary(), Args::[binary()]} |
    {Verb::binary(), Dobjstr::binary(),
     Prep::binary(), Iobjstr::binary(), Argstr::binary(), Args::[binary()]}.

%%%============================================================================
%%% API
%%%============================================================================

%%----------------------------------------------------------------------------
%% @doc Parses the binary string into a command specification tuple.
%%----------------------------------------------------------------------------
-spec parse(binary()) -> cmdspec().
parse(Data) ->
    token(Data, fun(Rest, Verb) -> 
        whitespace(Rest, fun(Rest2) ->
            command(Rest2, {Verb})
        end)
    end).

%%%============================================================================
%%% Internal functions
%%%============================================================================

%% Hacky! Returns everything from the binary until the first newline.
%% This is used to "strip" trailing newlines from the argstr.
-spec until_newline(binary()) -> binary().
until_newline(Data) ->
    until_newline(Data, <<>>).

-spec until_newline(binary(), binary()) -> binary().
until_newline(<<C,_Rest/binary>>, Acc) 
        when C =:= $\r; C =:= $\n -> Acc;
until_newline(<<C,Rest/binary>>, Acc) ->
    until_newline(Rest, <<Acc/binary, C>>);
until_newline(<<>>, Acc) -> 
    Acc.

-spec command(binary(), {binary()}) -> cmdspec().
command(Data, {Verb}) ->
    words(Data, fun(Rest, Dobj) -> 
        command(Rest, {Verb, Dobj, until_newline(Data)})
    end);
command(Data, {Verb, Dobj, Argstr}) ->
    case preposition(Data) of
        false -> 
            Dobjstr = oni_bstr:join(Dobj),
            Args = lists:concat([[Verb], Dobj]),
            {Verb, Dobjstr, Argstr, Args};
        {Prep, Rest} ->
            whitespace(Rest, fun(Rest2) -> 
                    command(Rest2, {Verb, Dobj, Prep, Argstr}) 
            end)
    end;
command(Data, {Verb, Dobj, Prep, Argstr}) -> 
    words(Data, fun(_Rest, Iobj) ->
        Dobjstr = oni_bstr:join(Dobj),
        Iobjstr = oni_bstr:join(Iobj),
        Args = lists:concat([[Verb], Dobj, [Prep], Iobj]),
        {Verb, Dobjstr, Prep, Iobjstr, Argstr, Args}
    end).

-spec words(binary(), fun()) -> any().
words(Data, Fun) ->
    words(Data, Fun, []).

-spec words(binary(), fun(), [binary()]) -> any().
words(<<>>, Fun, Acc) ->
    Fun(<<>>, lists:reverse(Acc));
words(Data, Fun, Acc) ->
    case preposition(Data) of
        {_, _} -> Fun(Data, lists:reverse(Acc));
        false -> 
            word(Data, fun(Rest, Word) ->
                whitespace(Rest, fun(Rest2) ->
                    words(Rest2, Fun, [Word|Acc])
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

-spec preposition(binary()) -> {binary(), binary()} | false.
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