---
layout: post
title: "Simple, Distributed and Scalable PubSub in Erlang"
description: "Building distributed messaging-based apps in Erlang using ErlBus."
date: 2016-01-13
categories: posts
tags: [Erlang, Messaging, Programming, Publish Subscribe]
comments: true
permalink: '/posts/2016/01/13/distributed-scalable-pubsub-in-erlang.html/'
feature_image: "https://unsplash.it/1200/400?image=524"
---

This blog post is about how to build high scalable and distributed messaging-based applications using [ErlBus](https://github.com/cabol/erlbus), which is a lightweight and simple library to enable what we want here.  
<!--more-->

Since current release `0.2.0` (in progress), **ErlBus** was improved substantially. The current **PubSub** implementation was taken from the original, remarkable, and proven [Phoenix PubSub Layer](https://hexdocs.pm/phoenix/Phoenix.PubSub.html),
but re-written in Erlang.

You may be wondering ¿why not to include Phoenix as dependency and call it from Erlang?. Well, there are some thoughts about it:

1. [Phoenix](http://www.phoenixframework.org) is a whole framework with different several modules, not only **PubSub**, so as a dependency, all sub-dependencies will be fetched too. Probably it isn't a big deal, but seems like we got all **Phoenix** and we only use a 5% (rest will be wasted). Besides, **PubSub** is a simple, small, and great piece of Software (architecture and design is pretty good), so the goal was to have only that single and specific module to handle messaging, not the whole web framework.

2. Maintainability. In this case, we wanted to have the change to maintain an lightweight **Erlang PubSub** version and evolve independently.

3. Build gets more complex, you will need not only **Erlang** but also **Elixir**, and make sure that all dependencies that **Phoenix** brings with it and yours get compiled well. Deal with a single platform is easier than deal with two or more.

## ErlBus Inside

As we explained before, **ErlBus** is the **Erlang** clone of [Phoenix PubSub](https://hexdocs.pm/phoenix/Phoenix.PubSub.html), so the architecture and design are exactly the same.

Due to **Phoenix PubSub** architecture, **ErlBus** scales out pretty well, not only globally (in cluster) but also locally. When we start an **ErlBus** instance, it starts **N** number of shards, and this value is set in the `pool_size` config parameter (by default `pool_size = 1`). In order to have a better idea of this, we should run **ErlBus**.

Let's build and start **ErlBus**:

{% highlight text %}
$ git clone https://github.com/cabol/erlbus.git
$ cd erlbus
$ make shell
{% endhighlight %}

Now we should have `ebus` (or **ErlBus**) running into an **Erlang** console. So now within the console let's run the `observer`:

{% highlight erlang %}
observer:start().
{% endhighlight %}

And, you should see a process tree like this:

<p align="center"><a href="#">
    <img src="{{ site.baseurl }}/assets/posts/ebus_process_tree_1.png" height="400" width="100%">
</a></p>
<p align="center"><span class="caption text-muted">ErlBus Process Tree.</span></p>

In the image above, we see the main supervisor `ebus_ps_pg2`, which supervises `ebus_ps_pg2_server` and the `ebus_ps_local_sup`. The `ebus_ps_local_sup` supervises the local shards, and each shard has its own supervisor `ebus_supervisor` which supervises the garbage collector `ebus_ps_gc` and the local **PubSub** server `ebus_ps_local`. Whole this runs on each instance of `ebus`.

Next step may be setup an **Erlang Cluster** using [Distributed Erlang](http://www.erlang.org/doc/reference_manual/distributed.html), and then we start `ebus` on each one, a [PG2](http://erlang.org/doc/man/pg2.html) group is created and each local `ebus_ps_pg2_server` is joined in order to received all broadcasted messages and then forward them locally. This is a pretty nice and highly scalable architecture.

Now let's see some examples!

## PubSub Example

Open an **Erlang** shell running `ebus`:

{% highlight text %}
$ make shell
{% endhighlight %}

Within the **Erlang** shell:

{% highlight erlang %}
% subscribe the current shell process
ebus:sub(self(), "foo").
ok

% spawn a process
Pid = spawn_link(fun() -> timer:sleep(infinity) end).
<0.57.0>

% subscribe spawned PID
ebus:sub(Pid, "foo").
ok

% publish a message
ebus:pub("foo", {foo, "hi"}).
ok

% check received message for Pid
ebus_proc:messages(Pid).             
[{foo,"hi"}]

% check received message for self
ebus_proc:messages(self()).             
[{foo,"hi"}]

% unsubscribe self
ebus:unsub(self(), "foo").
ok

% publish other message
ebus:pub("foo", {foo, "hello"}).
ok

% check received message for Pid
ebus_proc:messages(Pid).             
[{foo,"hi"},{foo,"hello"}]

% check received message for self (last message didn't arrive)
ebus_proc:messages(self()).             
[{foo,"hi"}]

% check subscribers (only Pid should be in the returned list)
ebus:subscribers("foo").
[<0.57.0>]

% check topics
ebus:topics().
[<<"foo">>]

% subscribe self to other topic
ebus:sub(self(), "bar").
ok

% check topics
ebus:topics().
[<<"bar">>,<<"foo">>]

% publish other message
ebus:pub("bar", {bar, "hi bar"}).
ok

% check received message for Pid (last message didn't arrive)
ebus_proc:messages(Pid).             
[{foo,"hi"},{foo,"hello"}]

% check received message for self
ebus_proc:messages(self()).             
[{foo,"hi"},{bar,"hi bar"}]
{% endhighlight %}

Now, let's make it more fun, start two Erlang consoles, first one:

{% highlight text %}
$ erl -name node1@127.0.0.1 -setcookie ebus -pa _build/default/lib/*/ebin -s ebus -config test/test.config
{% endhighlight %}

The second one:

{% highlight text %}
$ erl -name node2@127.0.0.1 -setcookie ebus -pa _build/default/lib/*/ebin -s ebus -config test/test.config
{% endhighlight %}

Then what we need to do is put these Erlang nodes in cluster, so from any of them send a ping to the other:

{% highlight erlang %}
% create a callback fun to use ebus_proc utility
CB1 = fun(Msg) ->
  io:format("CB1: ~p~n", [Msg])
end
#Fun<erl_eval.6.54118792>

% other callback but receiving additional arguments,
% which may be used when message arrives
CB2 = fun(Msg, Args) ->
  io:format("CB2: Msg: ~p, Args: ~p~n", [Msg, Args])
end.
#Fun<erl_eval.12.54118792>

% use ebus_proc utility to spawn a handler
H1 = ebus_proc:spawn_handler(CB1).
<0.70.0>
H2 = ebus_proc:spawn_handler(CB2, ["any_ctx"]).
<0.72.0>

% subscribe handlers
ebus:sub(H1, "foo").
ok
ebus:sub(H2, "foo").
ok
{% endhighlight %}

Repeat the same thing above in `node2`.

Once you have handlers subscribed to the same channel in both nodes, publish some messages from any node:

{% highlight erlang %}
% publish message
ebus:pub("foo", {foo, "again"}).
CB1: {foo,"again"}
CB2: Msg: {foo,"again"}, Args: "any_ctx"
ok
{% endhighlight %}

And in the other node you will see those messages have arrived too:

{% highlight erlang %}
CB1: {foo,"again"}
CB2: Msg: {foo,"again"}, Args: "any_ctx"
{% endhighlight %}

Let's check subscribers, so from any Erlang console:

{% highlight erlang %}
% returns local and remote subscribers
ebus:subscribers("foo").
[<7023.67.0>,<7023.69.0>,<0.70.0>,<0.72.0>]
{% endhighlight %}

To learn more about it you can go [HERE](https://github.com/cabol/erlbus).

So far, so good! Let's continue!

## Point-To-Point Example

The great thing here is that you don't need something special to implement a point-to-point behavior. It is as simple as this:

{% highlight erlang %}
ebus:dispatch("topic1", #{payload => "M1"}).
{% endhighlight %}

Let's see an example:

{% highlight erlang %}
% subscribe local process
ebus:sub(self(), "foo").
ok

% spawn a process
Pid = spawn_link(fun() -> timer:sleep(infinity) end).
<0.57.0>

% subscribe spawned PID
ebus:sub(Pid, "foo").
ok

% check that we have two subscribers
ebus:subscribers("foo").
[<0.57.0>,<0.38.0>]

% now dispatch a message (default dispatch fun and scope)
ebus:dispatch("foo", #{payload => foo}).
ok

% check that only one subscriber received the message
ebus_proc:messages(self()).
[#{payload => foo}]
ebus_proc:messages(Pid).
[]

% dispatch with options
Fun = fun([H | _]) -> H end.
#Fun<erl_eval.6.54118792>
ebus:dispatch("foo", <<"M1">>, [{scope, global}, {dispatch_fun, Fun}]).
ok

% check again
ebus_proc:messages(self()).                                         
[#{payload => foo}]
ebus_proc:messages(Pid).                                            
[<<"M1">>]
{% endhighlight %}

Extremely easy isn't?

## Summing Up

**ErlBus** is a simple and usable library that make thousands times easier to build: soft-real-time and highly scalable messaging-based applications.

It was implemented taken as base a remarkable and proven framework like [Phoenix Framework](http://www.phoenixframework.org).

Next step is create a coarse-grained interface on top of **ErlBus**, like [WebSockets](https://en.wikipedia.org/wiki/WebSocket), using [Cowboy](https://github.com/ninenines/cowboy). So may be something similar to [Phoenix Channels](http://www.phoenixframework.org/docs/channels), isn't?. See [WEST](https://github.com/cabol/west).

Finally, to learn more check [ErlBus](https://github.com/cabol/erlbus), and [ErlBus Examples](https://github.com/cabol/erlbus/tree/master/examples).
