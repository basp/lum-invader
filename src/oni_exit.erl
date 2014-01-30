-module(oni_exit).

-compile(export_all).    

move(Thing, Exit) ->
    %% 1. Check locks
    %% 2. Ask destination room for blessing using `accept'
    %% 3. If blessed, print `leave' and `oleave' messages
    %% 4. Move to destination room
    %% 5. Print `arrive' and `oarrive' messages for exit on 
    %%    the other side
    ok.