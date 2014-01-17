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
*   `oni_gender` has basic utilities to set gender. This is a requirement to make sure of the `ps` (pronoun substitution function) in the `oni_bstr` module.
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

So Oni runs things in a sequence. All players type commands, these get parsed into something called a `parsed_cmd` by the `oni_cmd` module and then that is resolved into something called a `package` by the `oni_pack` module. This `package` is then send to the `oni_rt_serv` process to be executed. At this point what happens depends mostly on how the verb is implemented. 

##### The Action Queue
Basic verbs like `l*ook` should be instant. Looking around doesn't take much longer than half a second so in game it is reasonably modelled like an instant action. You could however, create for example a `survey` action that spans a number of seconds. While doing such actions, you still want the user to be able to do instant actions (like `look`).

Oni supports this scenario out of the box and built right into the runtime. It is heavily inspired by the `actor` implementation in __HellCore__ and aims to offer the same sort of functionality. As we said earlier, every user is assigned a `oni_aq_serv` process that is his or her (or _its_ even) personal action queue. This queue will even survive reconnects as long as the Oni app is running so it should be only spawned once for each user. 

To test out the action queue, let's write a simple action verb. These consist of three functions: a `start_something` function and a `finish_something` function that form the action chain and a `something` function that queues the `start_something` verb. This sounds more complicated than it really is so let's implement the `survey` verb in the `sandbox` module.

This will create a new `package` for the `start_survey` function and queue that on the player action queue. We don't want the runtime itself to _know_ all verbs so the burden of queuing actions is on the verb implementors:

    survey(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        Pack = oni_pack:create({sandbox, start_survey}, Bindings),
        oni_aq_sup:queue(Player, Pack).

This will start the `survey` action. Long running actions are  implemented as continuations so we can have quick bursts of activity on `oni_rt_serv` and a lot of suspending of individual processes (we don't want `oni_rt_serv` to block).

    start_survey(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        oni:notify(Player, "You start surveying your surroundings."),
        %% We return a continue tuple with a sleep time (3000 ms)
        %% and a continuation MFA (Module, Function, Args). 
        {continue, 3000, {sandbox, finish_survey, [Bindings]}}.

This will finish up the `survey` action. The `start` and `finish` pattern is very basic but useful. There are more patterns available though if you have more complicated actions (more on those later).

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

You can now execute the `survey` command from your telnet shell. Let's see what we can do with it:

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

We could implement a more fancy action by having a `continue` stage. First change the `start_survey` function to look like this:

    start_survey(Bindings) ->
        Player = proplists:get_value(player, Bindings),
        oni:notify(Player, "You start surveying your surroundings."),
        {continue, 3000, {sandbox, continue_survey, [Bindings]}}.

We only changed the continuation tuple to specify `continue_survey` instead of `finish_survey`. We have to implement the `continue_survey` function though:

    continue_survey(Bindings) ->
        case random:uniform() > 0.555 of
            true ->
                Player = proplists:get_value(player, Bindings),
                oni:notify(Player, "You find some interesting terrain."),
                {continue, 5000, {sandbox, continue_survey, [Bindings]}};
            false ->
                {continue, 1000, {sandbox, finish_survey, [Bindings]}}
        end.

With this extra part there is a slight chance of finding _interesting terrain_ which slows down the survey. If we don't find anything interesting we just look around for a little bit and then finish.

Now if you execute this from the shell, every once in a while you will see this:

    survey
    You start surveying your surroundings.
    You find some interesting terrain.
    You finish surveying your surroundings.

That spices up our action a little bit. Not that our actions are very _light-weight_ on processor time. We get some stuff from bindings, notify the player and then return with a `continue` tuple or any other result to signal done. Your outside actions should be very thin. If there is any heavy lifting involved you should consider implementing it as a seperate module or maybe even a `port`. That being said, _just Erlang_ should be fast enough for MOO purposes. 

#### Performance and Storage
Oni as of yet is not designed to run on multiple nodes. This is unfortunate but a typical MOO server does not really need the capacity of a distributed computing platform (unless you get really popular and in that case at least you are in Erlang already so it's not that bad). Eventually it would be nice to have a distributed Oni but this is not a high priority.

This means we have to consider performance of critical components in the __game loop__ (Oni doesn't really have a game loop as such but the `oni_rt_serv` comes close). One of the interesting things is that we can do all command processing outside of the game loop so each player has a seperate process that listens to the socket, parses commands and resolves them into packages. These things can all be done outside of the main `oni_rt_serv` processs.

The whole Oni database resides in memory as ETS tables and everything that needs to be persisted (or shared) in any way is in those tables. The tables are easily recognizable in `tv` as they are prefixed by `oni_`. Currently, there are four tables that Oni uses:

*   `oni_action_queues` stores the relation between `player` object ids and `oni_aq_serv` process identifiers. The API to this table is in the `oni_aq` module.
*   `oni_connections` stores the relation of `player` objects and their connections. We want to be able to `notify` objects instead of raw sockets (which are fine in the runtime but not so much in verb implementations) and this table provides that association.
*   `oni_counters` is a counter table that contains simple integers. For now it only holds the object id value that is used to give each object an unique id.
*   `oni_objects` contains the main object database. This hold all the objects, their properties and their verb pointers.

Because everything is in ETS tables stuff is extremely fast but also very volatile. If `oni_sup` goes you are probably doomed although most problems are fixable (with due effort). In the future we plan to supply backup functionaly to save and restore Oni state but for now we can use the available functions that Erlang supplies to deal with saving and restoring ETS tables. This is not a high priority yet though as Oni is not finished and the final database design might change (slim chance though).

I can't even time parsing and packaging commands reasonably on this crappy laptop so that is quite fast. The runtime is sequential so its speed only depends on how clever you design your verbs. More guidelines on that are planned for the future as the engine is becoming more developed.