---
layout: post
title: "Testing Distributed Apps with Common Test"
date: 2015-12-11 13:00
comments: true
categories: erlang common-test distribued-apps
author: Carlos Andres Bolaños
ispost: true
abstract: "Testing Distributed Apps with Common Test"
feature_image: "https://unsplash.it/1200/400?image=1078"
---

This post is about a very common scenario: how to test distributed applications in Erlang? Well, fortunately, Erlang has a remarkable testing framework: [Common Test](http://www.erlang.org/doc/man/common_test.html). With CT it is possible to achieve it, and the best part is that it's extremely easy.
<!--more-->

The best way to explain how to test a distributed app with CT is with as an example, so we're going to use the project [ToyKV](https://github.com/inaka/toy_kv) as example. **ToyKV** is a simple/reduced distributed Key/Value store. In a distributed Key/Value store, reads and writes are spread across multiple KV nodes, so it would be nice to have tests that allow us to recreate this scenario, start multiple Erlang nodes running `toy_kv` app, and execute the provided API operations on these nodes transparently.

Because our application is distributed by nature, the issue here relies on how to spawn multiple Erlang nodes from CT. To do this, we can use the [ct_slave](http://www.erlang.org/doc/man/ct_slave.html) module provided by CT. With `ct_slave` we can spawn multiple nodes from the CT suites.

Returning to `toy_kv`, what we want is to be able to start five KV nodes; we already have one, the master, which is the node from where the test suites are run, so we have to add a function in the suite to spawn the other four (a, b, c, d), and this function may be called from the `init_per_suite/1` CT function.

{% highlight erlang %}
start_slaves() ->
  Nodes = [a, b, c, d],
  start_slaves(Nodes, []).

start_slaves([], Acc) -> Acc;
start_slaves([Node | T], Acc) ->
  ErlFlags =
    "-pa ../../_build/default/lib/*/ebin " ++
    "-config ../../test/dist_test.config",
  {ok, HostNode} = ct_slave:start(Node,
    [{kill_if_fail, true},
     {monitor_master, true},
     {init_timeout, 3000},
     {startup_timeout, 3000},
     {startup_functions,
      [{toy_kv, start, []}]},
     {erl_flags, ErlFlags}]),
  ct:print(
    "\e[32m Node ~p [OK] \e[0m", [HostNode]),
  pong = net_adm:ping(HostNode),
  start_slaves(T, [HostNode | Acc]).
{% endhighlight %}

As it's shown in the snippet of code above, to spawn a slave node, the function `ct_slave:start/2` is called. The first argument is the name of the node and the second one is an option list allowed by `ct_slave`. The options used in this example are:

* `{startup_functions, StartupFunctions}`: here you have to specify the start function of your app, in our case is `{toy_kv, start, []}`.
* `{erl_flags, ErlangFlags}`: here you pass the arguments to start the Erlang runtime system.
* `{kill_if_fail, KillIfFail}`: specifies if the slave node should be killed in case of a timeout during initialization or startup.
* `{monitor_master, Monitor}`: specifies if the slave node should be stopped in case the master node stop.
* `{init_timeout, InitTimeout}`: time to wait for the node until it calls the internal callback function informing master about successful startup.
* `{startup_timeout, StartupTimeout}`: time to wait until the node finishes to run the StartupFunctions.

You can check the detailed list of options [HERE](http://www.erlang.org/doc/man/ct_slave.html).

Once we call `ct_slave:start/2`, the node is started and the next step, which is optional, is to execute a ping to that started node, using `net_adm:ping(HostNode)`. The ping step is done to guarantee the started node is running correctly.

Then the test cases are executed using the started KV nodes. Now we have a test recreating a real distributed scenario.

To have a better and deeper understanding about this, you can check the source code of `toy_kv` [HERE](https://github.com/inaka/toy_kv). Within the `test` folder you'll find the distributed test suite `dist_SUITE.erl`.
