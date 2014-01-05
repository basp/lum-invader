-module(oni_cmd).

-compile(export_all).

-spec whitespace(binary(), fun()) -> any().
whitespace(<<C, Rest/binary>>, Fun) when C =:= $\s; C =:= $\t ->
    whitespace(Rest, Fun);
whitespace(Data, Fun) ->
    Fun(Data).

-spec tokens(binary()) -> [binary()].
tokens(Data) ->
    tokens(Data, []).

-spec tokens(binary(), [binary()]) -> [binary()].
tokens(<<>>, Acc) -> lists:reverse(Acc);
tokens(Data, Acc) ->
    token(Data, fun(Rest, T) ->
        whitespace(Rest, fun(Rest2) ->
            case Rest2 of
                <<$", Rest3/binary>> -> 
                    quoted_string(Rest3, fun(Rest4, T2) ->
                        tokens(Rest4, [T2,T|Acc]) 
                    end);
                _ -> tokens(Rest2, [T|Acc])
            end
        end)
    end).

-spec token(binary(), fun()) -> any().
token(Data, Fun) ->
    token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) -> 
    Fun(<<>>, Acc);
token(Data = <<C, _Rest/binary>>, Fun, _Case, Acc) 
            when C =:= $\s; C =:= $\t ->
    Fun(Data, Acc);
token(<<C, Rest/binary>>, Fun, Case = ci, Acc) ->
    token(Rest, Fun, Case, <<Acc/binary, C>>);
token(<<C, Rest/binary>>, Fun, Case = cs, Acc) ->
    token(Rest, Fun, Case, <<Acc/binary, C>>).

-spec quoted_string(binary(), fun()) -> any().
quoted_string(Data, Fun) ->
    quoted_string(Data, Fun, <<>>).

-spec quoted_string(binary(), fun(), binary()) -> any().
quoted_string(<<>>, _Fun, _Acc) ->
    {error, badarg};
quoted_string(<<$", Rest/binary>>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<<$\\, C, Rest/binary>>, Fun, Acc) ->
    quoted_string(Rest, Fun, <<Acc/binary, C>>);
quoted_string(<<C, Rest/binary>>, Fun, Acc) ->
    quoted_string(Rest, Fun, <<Acc/binary, C>>).