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

#### Forking and suspending
LambdaMOO supports forking and suspending in the form of `fork` and `suspend`. A `fork` creates a parallel task with an id and optionally a timer (number of seconds before it starts). A `suspend` just suspends the current task for the given number of seconds. In LambdaMOO these are quite low level constructs.

Although the __action queue__ is the recommended construct for utilizing actions that span time, sometimes you need to do more low level stuff.