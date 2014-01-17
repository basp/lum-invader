## 鬼ラム・インベーダ
This is Oni, the next version. Focus is on design, API, tests and documentation.

#### Overview
There are a few OTP modules in Oni:

*   `oni_sockserv_serv` is the main user socket listener, it will listen to incoming data and send it off for further processing.
*   `oni_sockserv_sup` is the supervisor for the above, it will spawn child processes for `oni_sockserv_serv` and keep track of them.
*   `oni_aq_serv` is the __action queue__ server. One of these will be spawned for each individual `player`.
*   `oni_aq_sup` is the supervisor for `oni_aq_serv`. It keeps track of any `oni_aq_serv` processes that are spawned.
*   `oni_aq` is in charge of the ETL table that connects action queue processes to player object ids. 
*   `oni_rt_serv` is the main Oni runtime. We need to execute a lot synchronously (in order) to alleviate the need to sync everything in each individual verb. This server component will execute all verb commands from all users in order. Of course this also means it will be a big bottleneck.
*   `oni_sup` is the main supervisor.
*   `oni_app` is the appplication that ties everything together.
*   `oni_event` is the event module with a basic listener in the form of `oni_event_logger`.

Apart from all the OTP stuff, there's also a lot of utility and support modules:
*   `oni_db` is quite a big module that handles all object manipulations. It is used to create objects, move them around, add and set properties and verbs and also permission flags.
*   `oni_cmd` is used to do raw command parsing on incoming user request data.
*   `oni_pack` is used to package up a parsed command into something that can be evaluated by the runtime. This basically means that the verb is resolved to code and that all the arguments are resolved to object references.
*   `oni_match` contains some utility methods to do matching on various kinds of things in the runtime.
*   `oni_bstr` has routines to deal with binaries. A lot of string handling in Oni is done in the form of binaries and this module supplies some helpers.
*   `oni_ansi` has ANSI color code support to apply styles and also to strip them.
*   `oni_who` is a module that can be used to find information (active) players.
*   `oni` has some core functions that are useful to most of the other modules.

There might be some other random stuff like `oni_test_world` that has some code to setup a basic world implementation to test out various things but the above is mostly it.

#### Installing
Oni (lum invader) is a standard OTP application, you can unzip, compile everything from `src` to `ebin` with 'erlc -o ebin ./src/*.erl` and that's basically it.

#### Bootstrapping
Start Erlang with `erl -pa ebin` and it should return with the standard Erlang shell prompt. First, we need to start to application:

    1> oni:start().
    ok
    2>

You can make sure everything is running with `appmon:start()` but if you get `ok` you're safe to assume that everything went as expected.

In Oni, the first player object cannot be created from a telnet session. That is because we don't have to - we already have Erlang shell that we can use.

    2> Wiz = oni_db:create(nothing).
    1
    3>

The above creates an object with a new unique id. The `nothing` argument is the `parent` parameter but we don't have to worry about that for now. The `oni_db:create` method responds with `1`. That is the id of our newly created object.

At this point we can't quite login to that object yet. We have to set the `player` flag and we also have to `name` the object:

    3> oni_db:set_player_flag(Wiz, true).
    true
    4> oni_db:rename(Wiz, <<"Wiz">>).
    true

Now we should be able to login to `Wiz` (Oni doesn't really do passwords yet, it's not ready for public). Just bootup your favorite telnet (or preferably MUD) client and connect to the local Oni instance (`127.0.0.1:7777` if you didn't change anything).
   
    Oni [lum invader]
    <<255,252,3,255,250,201,67,111,114,101,46,83,117,112,112,111,114,116,115,46,83,101,116,32,91,93,255,240>>

The random binary output is capability info that is currently ignored by Oni although we have a state for it in `oni_sockserv_serv`. Please ignore it for now and `connect Wiz`:

    connect Wiz

And Oni should respond with:

    *** connected (Wiz) ***

Congrats! You are now connected to a player object in the Oni database and some cool stuff has happened. We assigned you a record in the `who` table and also you have been given your very own `oni_aq_serv` process. If you fire up `appmon` and `tv` you can view the these.

Wiz cannot do much yet. Everything you do will be responded with something like:

    [huh?] {parsed_cmd,<<"foo">>,<<>>,[],<<>>,nothing,<<>>,<<>>,nothing}

This is a raw command representation from Oni that basically is development/debug output and means - I don't understand you. So how to make Oni understand you? Add verbs of course! But before we get to that, let's take a brief look at some of Oni's telnet `wizard` and `programmer` support.

#### Evaluating Code
You cannot add verbs yet from your telnet/MUD client but you could if you wanted to. Just execute the following from your erlang shell first:

    5> oni_db:set_wizard_flag(Wiz, true).
    true
    6> oni_db:set_programmer_flag(Wiz, true).
    true

That will make you a `wizard` and `programmer` which allows you to evaluate code and do the following from your telnet client (note the `;` prefix):

    ;2+3.
    => 5

Don't type the `=> 5` (that is just the shell responding). This allows you to evaluate any Erlang expression list so be careful, it's great for trusted people though because they have now access to all the Oni API too:

    ;oni_db:create(nothing).
    => 2

You can even "remember" values by assinging them:

    ;Foo = "foo".
    => "foo"
    Foo.
    => "foo"

If you wanna reset your bindings (your values) just type `@reset` and everyting will be cleared:

    @reset
    Environment reset.
    ;Foo.
    error: {unbound,'Foo'}

This is very powerful but also __very dangerous__. Oni is __not meant to go public__ yet so __be careful__ with this.

#### Basic Verbs
Let's work from the Erlang shell because it's easier to follow. However, you could do all of this from your telnet session too if you wanted.

    7> oni_db:add_verb(Wiz, {Wiz, [<<"l*ook">>]}, {none, none, none}).
    true

This adds a verb to the `Wiz` object. The second `Wiz` means that `Wiz` is also the owner of the verb (this is not important yet). The list of binaries is the names of the verb. The `*` is a wildcard specifier that will be explained later but functions exactly the same as the LambdaMOO equivalent. the `{none, none, none}` specifier are the verb args which will also be explained later. Note that these too function exactly as they do in LambdaMOO.

If you now try to `look` from your telnet client you can see that your connection crashes. If you look very carefuly in your Erlang shell you can see that it freaks out on `{badmatch,none}`. To understand why this is happening just execute the following:

    8> oni_db:verb_code(Wiz, 1). 
    none

And you'll see it's `none`. Just like the `badmatch` above. To solve this we have to create some Erlang code for this verb. Create a new Erlang module called `sandbox.erl` or whatever you want and add the following:

    -module(sandbox).
    -compile(export_all).

    look(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        oni:notify(Player, "You look around.").

This is a very simple verb implementation but we can always extend it later very easily while the server is running. We just need to connect this to the Wiz object's `l*ook` verb. First we need to compile this new module and after that we need to load it in our Erlang shell. After this is done we can do the following:

    9> oni_db:set_verb_code(Wiz, 1, {sandbox, look}).
    true

In Oni, each verb points to a function with a `{Module, Function}` tuple in the verb definition. The function should take a single parameter `Bindings` that contains the initial arguments that are supplied by the Oni runtime. This contains stuff like `player`, `this`, and `args`.

Now that we have that verb pointing to a bit of code we can execute the look command from our telnet client:

    look
    You look around.

If you still get horrible crashes make sure you compile the module and __load it into the shell__ with `l(sandbox)` first. Then just reconnect and try again (making sure to `connect Wiz` first).

#### The Oni Runtime
LambdaMOO supports forking and suspending in the form of `fork` and `suspend`. A `fork` creates a parallel task with an id and optionally a timer (number of seconds before it starts). A `suspend` just suspends the current task for the given number of seconds. In LambdaMOO these are quite low level constructs.

Oni supports the same functionality (and much more) but at first it might not be obvious how to translate a LambdaMOO `fork` or `suspend` to a proper Oni runtime construct. Before we can explain how to implement those constructs in Oni though we have to explain a bit on how things are executed within the Oni runtime.

##### Synchronous Verbs
All execution of verbs that are a direct consequence of parsing a player supplied command string are executed in a single stream by a single process. This process is the `oni_rt_serv` process and from our perspective it has one useful function: `exec/1`. This is our entry point into the execution stream.

So why have this bottleneck at all? Why not have players run their processes just as is - in individual processes? Well, the main thing has to do with object manipulation in Oni and the way on how we want users to implement verbs. If we got rid of the `oni_rt_serv` and just executed everything concurrently we have to make sure that each verb runs appropriate pieces of code inside a transaction thus placing the burden of writing conflict free code on the verb implementors.

So Oni runs things in a sequence. All players type commands, these get parsed into something called a `cmdspec()` and then that command specification is resolved into something called a `package`. This `package` is then send to the `oni_rt_serv` process to be executed. At this point what happens depends mostly on how the verb is implemented. 

##### The Action Queue
Basic verbs like `l*ook` should be instant. Looking around doesn't take much longer than half a second so in game it is reasonably modelled like an instant action. You could however, create for example a `survey` action that spans a number of seconds. While doing such actions, you still want the user to be able to do instant actions (like `look`).

Oni supports this scenario out of the box and built right into the runtime. It is heavily inspired by the `actor` implementation in __HellCore__ and aims to offer the same sort of functionality. As we said earlier, every user is assigned a `oni_aq_serv` process that is his or her (or _its_ even) personal action queue. This queue will even survive reconnects as long as the Oni app is running so it should be only spawned once for each user. 

To test out the action queue, let's write a simple action verb. These consist of three functions: a `start_something` function and a `finish_something` function that form the action chain and a `something` function that queues the `start_something` verb. This sounds more complicated than it really is so let's implement the `survey` verb in the `sandbox` module:

    survey(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        Pack = oni_pack:create({sandbox, start_survey}, Bindings),
        oni_aq_sup:queue(Player, Pack).

    start_survey(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        oni:notify(Player, "You start surveying your surroundings."),
        %% We return a continue tuple with a sleep time (3000 ms)
        %% and a continuation MFA (Module, Function, Args). 
        {continue, 3000, {sandbox, finish_survey, [Bindings]}}.

    finish_survey(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        oni:notify(Player, "You finish surveying your surroundings.").        

Now compile the `sandbox` module and reload it into the running erlang shell:

    13> l(sandbox).
    {module,sandbox}

And then add the verb and verb code to the `Wiz` object:

    14> oni_db:add_verb(Wiz, {Wiz, [<<"surv*ey">>]}, {none, none, none}).
    true
    15> oni_db:verbs(Wiz).  
    [<<"surv*ey">>,<<"l*ook">>]
    16> oni_db:set_verb_code(Wiz, 1, {sandbox, survey}).
    true

Note that the newly added verb has index `1` in the verb list. This makes it easy to setup verbs in script as you can be sure that the last added verb is the first one in the list. 

You can now execute the `survey` command from your telnet shell. Let's see what we can doo with it:

    survey
    You start surveying your surroundings.
    look
    You look around.
    survey
    [ queued - 'survey' ]
    survey
    [ queued - 'survey' ]
    You finish surveying your surroundings.
    You start surveying your surroundings.
    You finish surveying your surroundings.
    You start surveying your surroundings.
    You finish surveying your surroundings.

In the above you can see that while surveying, we can still look around. However, if we try to `survey` while we are already surveying you will see that the action is queued. When we initial survey finishes the next action from the queue is picked up and so on until we eventually run out of actions to execute.