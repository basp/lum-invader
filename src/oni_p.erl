-module(oni_p).

-export([
    result/1, item/0, zero/0, bind/2, seq/2, sat/1, 
    char/1, digit/0, upper/0, lower/0, string/1,
    letter/0, alphanum/0, word/0, plus/2, many/1,
    many1/1]).

-type parser() :: fun((binary()) -> [{any(), binary()}]).

-spec result(any()) -> parser().
result(V) -> 
    fun(Data) -> [{V, Data}] end.

-spec zero() -> parser().
zero() -> 
    fun(_Data) -> [] end.

-spec item() -> parser().
item() ->
    fun(Data) ->
        case Data of
            <<>> -> [];
            <<X, XS/binary>> ->  [{X, XS}]
        end
    end.

-spec bind(parser(), fun((any()) -> parser())) -> parser().
bind(P, F) -> 
    fun(Data) -> 
        lists:concat([(F(V))(D) || {V, D} <- P(Data)]) end.

-spec seq(parser(), parser()) -> parser().
seq(P, Q) ->
    bind(P, fun(X) -> 
        bind(Q, fun(Y) -> 
            result({X, Y}) end) end).

-spec sat(fun((char()) -> boolean())) -> parser().
sat(P) ->
    Q = fun(X) -> 
        case P(X) of 
            true -> result(X); 
            false -> zero() 
        end
    end,
    bind(item(), Q).

-spec plus(parser(), parser()) -> parser().
plus(P, Q) ->
    fun(Data) -> lists:concat([P(Data), Q(Data)]) end.

-spec char(char()) -> parser().
char(X) ->
    sat(fun(Y) -> X =:= Y end).

-spec digit() -> parser().
digit() ->
    sat(fun(X) -> $0 =< X andalso X =< $9 end).

-spec lower() -> parser().
lower() ->
    sat(fun(X) -> $a =< X andalso X =< $z end).

-spec upper() -> parser().
upper() ->
    sat(fun(X) -> $A =< X andalso X =< $Z end).

-spec letter() -> parser().
letter() ->
    plus(lower(), upper()).

-spec alphanum() -> parser().
alphanum() ->
    plus(letter(), digit()).

-spec string(string()) -> parser().
string("") -> result("");
string([X|XS]) ->
    bind(char(X), fun(_) -> 
        bind(string(XS), fun(_) -> 
            result([X|XS]) end) end).

-spec word() -> parser().
word() ->
    Word = bind(letter(), fun(X) ->
        bind(word(), fun(XS) ->
            result([X|XS]) end) end),
    plus(Word, result("")).

-spec many(parser()) -> parser().
many(P) ->
    Many = bind(P, fun(X) ->
        bind(many(P), fun(XS) ->
            result([X|XS]) end) end),
    plus(Many, result("")).

-spec many1(parser()) -> parser().
many1(P) ->
    bind(P, fun(X) ->
        bind(many(P), fun(XS) ->
            result([X|XS]) end) end).