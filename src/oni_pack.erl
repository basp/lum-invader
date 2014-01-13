-module(oni_pack).

-compile(export_all).

cmd(Cmd, User) ->
    Verbstr = oni_cmd:verbstr(Cmd),
    case oni_db:verb_code(User, Verbstr) of
        {M, F} -> {M, F};
        X -> X %% for now, should probably invoke "huh" or something
    end.

-spec lookup_verb(Str::binary(), {User::oni_cmd:objid()}) -> 
    {This::oni_cmd:objid(), {M::atom(), F::atom()}} | none.
lookup_verb(Str, {User}) ->
    