## Lum Invader
This is Oni, the next version. Focus is on design, API, tests and documentation.

#### Installing
Oni (lum invader) is a standard OTP application, you can unzip, compile everything from `src` to `ebin` with 'erlc -o ebin ./src/*.erl` and that's basically it.

#### Bootstrapping
Start Erlang with `erl -pa ebin` and it should return with the standard Erlang shell prompt. First, we need to start to application:

    1> oni:start().
    ok
    2>

You can make sure everything is running with `appmon:start()` but if you get `ok` you're safe to assume that everything went as expected.

#### Connecting
After executing `oni:start().` the server will be running on port `7777`. We can connect to it with any telnet client (I'm using [Mudlet](http://www.mudlet.org/) here). After connecting to the server I'm getting the following output:

    Oni [lum invader]
    {<<255,252,3,255,250,201,67,111,114,101,46,83,117,112,112,111,114,116,115,46,
       83,101,116>>,
     <<"[]">>,<<"[]">>,
     [<<"[]">>]}

In the above, my client (Mudlet) is sending some stuff to the server while it is in the connecting state. We don't quite know what to make of it so we try to parse it as a player command. The default (development) behaviour is to return a `cmdspec()` tuple for debugging purposes and that is what we are seeing. Your client might not be sending anything or it might be sending a bunch of other bytes. 

We can try sending it some more reasonable stuff:

    take silver bullet from shady toolkit
    {<<"take">>,<<"silver bullet">>,<<"from">>,<<"shady toolkit">>,
     <<"silver bullet from shady toolkit">>,
     [<<"silver">>,<<"bullet">>,<<"from">>,<<"shady">>,<<"toolkit">>]}

That gives us an inside view on how commands are handled at a low level. We get another `cmdspec()` but instead of some bytes we cannot print we have some more reasonable output. A command specification is a raw representation of the thing that the server is going to run and the arguments that are going to be passed to it.

