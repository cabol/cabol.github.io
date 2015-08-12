---
layout: post
title: "ErlBus: Erlang Message Bus"
description: "Highly scalable pub/sub message bus written in Erlang"
date: 2015-07-26
tags: [Erlang, Messaging, Programming, Publish Subscribe]
image:
  feature: abstract-2.jpg
---

Messaging in [Erlang](http://www.erlang.org) is easy by default, because of its nature. But, why not make it even easier? ridiculously easy, and fun. Well, this is where [ErlBus](https://github.com/cabol/erlbus) comes in; an Open Source project created to provide all what you need around messaging, including quality attributes such as: high performance and scale-out.

Let's start doing an implementation of a simple but very powerful messaging pattern: [Publish/Subscribe](http://www.enterpriseintegrationpatterns.com/PublishSubscribeChannel.html). In Erlang there are different ways to do it, but I'm going to mention only two (the common ones):

1. Using the native tool/library [pg2](http://erlang.org/doc/man/pg2.html).
2. Using an external tool/library like [gproc](https://github.com/uwiger/gproc).

###Pub/Sub with pg2

**pg2** doesn't provides explicit Pub/Sub functions, so we have to implement that logic ourselves, using the given primitives.

{% highlight text %}
%% 1st: we have to create a pg2 group, that will be the analogous to a pub/sub channel
1> ok = pg2:create(my_channel).
ok
%% 2nd: subscribe the current process to that group/channel
2> ok = pg2:join(my_channel, self()).
ok
%% 3rd: publish one message, but we have to get the subscribers list first,
%% and then proceed with the broadcast
3> Subscribers = pg2:get_members(my_channel).
4> lists:foreach(fun(Pid) -> Pid ! "Hello!" end, Subscribers).
ok
%% 4th: get the received message
5> receive Msg -> Msg end.
"Hello!"
{% endhighlight %}

---

Things to highlight here:

- Is easy, but you have to code more and deal with some annoying things like errors or exceptions.

- We have to code the logic for receive incoming messages (a listener), in addition to the message handling logic, and we should only be concerned for handle/process our messages.

- You will be violating the [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) principle soon, because you'll end up coding the same thing over and over again.

###Pub/Sub with gproc

With **gproc** happen almost the same situation, but at least provides a pub/sub module `gproc_ps` to abstract that logic better.

{% highlight text %}
%% 1st: start gproc application
1> application:start(gproc).
ok
%% 2nd: subscribe the current process to that group/channel
2> gproc_ps:subscribe(l, my_channel).
true
%% 3rd: publish one message
3> gproc_ps:publish(l, my_channel, "Hello!").
{gproc_ps_event,my_channel,"Hello!"}
%% 4th: get the received message
5> receive Msg -> Msg end.
{gproc_ps_event,my_channel,"Hello!"}
{% endhighlight %}

---

Almost the same thing is it? with some minor differences of course. Again, we could highlight exactly the same points as previously with **pg2**.

Either with **gproc** or **pg2** we're limited to the pub/sub basics. We don't have:

- Event-driven consumers that allow us implement the message handling logic.

- Task executors or worker pools, in case that we need distribute load across of a pool of workers.

- Point-To-Point patter.

- Different scaling strategies, different to `pg2` global model, or `gproc` with `gen_leader`.

And other things, but don't worry, we'll cover all these things later.

###Pub/Sub with ErlBus

Now we are on the heart of all this, **ErlBus** is a very lightweight and simple approach to build messaging-based apps. Messaging infrastructure that provides: Publish/Subscribe, Point-To-Point, Event Driven Consumer (Message Handler), Task Executor, etc., all that is missing in the previously explored options, all what you really need and expect from a messaging tool.

**ErlBus** has two main modules:

- `ebus` which provides all pub/sub and messaging basics in general.

- `ebus_handler` that provides all needed functions to create and manage message handlers.

As we mentioned, one of the big differences and also strength, is the ability to have real event-driven consumer or message handlers, so let's start creating our handler.

**my_handler.erl**

{% highlight erlang %}
-module(my_handler).

-behaviour(ebus_handler).

%% API
-export([handle_msg/2]).

handle_msg({Channel, Msg}, Context) ->
  io:format("[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n",
            [self(), Channel, Msg, Context]).
{% endhighlight %}

---

Also, if you don't like to create a module to implement `ebus_hanlder` behavior, **ErlBus** gives you the chance to pass an anonymous function to act as callback. In the example below you can see the mentioned ways to create and manage message handlers (anonymous funs or implementing `ebus_hanlder` behaviour).


{% highlight text %}
%% Start ebus
1> application:start(ebus).
ok

%% Create first handler
2> MH1 = ebus_handler:new(my_handler, <<"MH1">>).
<0.50.0>

%% Now let's create another but using an anonymous function as callback
3> F = fun({Channel, Msg}, Ctx) ->
3>       io:format("[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n",
3>                 [self(), Channel, Msg, Ctx])
3>     end.
#Fun<erl_eval.12.90072148>
4> MH2 = ebus_handler:new(F, <<"MH2">>).
<0.52.0>

%% Subscribe them to channel ch1
5> ebus:sub(ch1, [MH1, MH2]).
ok

%% Let's publish a message to 'ch1'
6> ebus:pub(ch1, "Hello!").
ok
[Pid: <0.50.0>][Channel: ch1][Msg: "Hello!"][Ctx: <<"MH1">>]
[Pid: <0.52.0>][Channel: ch1][Msg: "Hello!"][Ctx: <<"MH2">>]

%% Unsubscribe 'MH2' from ch1
7> ebus:unsub(ch1, MH2).
ok

%% Publish again to 'ch1'
8> ebus:pub(ch1, "Hello again!").
ok
[Pid: <0.50.0>][Channel: ch1][Msg: "Hello again!"][Ctx: <<"MH1">>]
{% endhighlight %}

---

How was it? awesome isn't? Well, let's finish giving another interesting example.

Suppose now that you have a handler that takes a while processing each message/event, so it will be blocked until complete the task, and for some scenarios would be unthinkable. Therefore, `ebus_handler` module gives you the option to create a pool of workers attached to your handler, and is totally transparent to you.

{% highlight text %}
%% Start ebus
1> application:start(ebus).
ok

%% Create a handler with a worker pool (3 workers)
2> Pool1 = ebus_handler:new_pool(my_pool_1, 3, my_handler).
<0.49.0>

%% Let's create another pool but using an anonymous function as callback
3> F = fun({Channel, Msg}, Ctx) ->
3>       io:format("[Pid: ~p][Channel: ~p][Msg: ~p][Ctx: ~p]~n",
3>                 [self(), Channel, Msg, Ctx])
3>     end.
#Fun<erl_eval.12.90072148>
4> Pool2 = ebus_handler:new_pool(my_pool_2, 3, F).
<0.52.0>

%% And that's it, now the load will be distributed among the workers
%% From here everything is as previously
%% Finally, let's subscribe these new pools with workers to some channel
5> ebus:sub(my_channel, [Pool1, Pool2]).
ok
{% endhighlight %}

---

Once the pool is subscribed to a channel and some message arrives, it will be processed only by one worker of that pool; this is one way to have a Point-To-Point behavior.

##Distributed ErlBus

**ErlBus** is distributed by nature, inherits all properties of [Distributed Erlang](http://www.erlang.org/doc/reference_manual/distributed.html) and [pg2](http://erlang.org/doc/man/pg2.html). But `pg2` has some limitations, distribution model works with full replication, which can cause problem when we have a considerable amount of subscribers, and at the same time the amount of messages sent is too high. For these scenarios **ErlBus** provides another option: `ebus_dist`, which is built on top of [riak_core](https://github.com/basho/riak_core) and [gproc](https://github.com/uwiger/gproc).

There are more nice features, so I strongly recommend you visit [ErlBus GitHub](https://github.com/cabol/erlbus), you will find more documentation and examples.

##Summing Up

Is extremely important let something clear, **ErlBus** is not a Message Queue System or [MoM](https://en.wikipedia.org/wiki/Message-oriented_middleware) like [RabbitMQ](https://www.rabbitmq.com/). In the **MoM**, channels are represented by queues and messages always cross the **MoM**. With **ErlBus**, [channels](http://www.enterpriseintegrationpatterns.com/MessageChannel.html) are just a logical mechanism to allow communicate two or more endpoints (processes) each other, and messages are directly delivered from senders/publishers to receivers/subscribers; messages arrives to the buffer of each process. According to that, **ErlBus** and **RabbitMQ** drives different problems and applies to different scenarios.

**ErlBus** is just a simple and lightweight tool to build messaging-based applications, which provides an easy and usable interface on top of known and proven libs/tools like [pg2](http://erlang.org/doc/man/pg2.html), [gproc](https://github.com/uwiger/gproc) and [riak_core](https://github.com/basho/riak_core), enabling a clearer and more powerful semantics for messaging patterns such as: Publish/Subscribe, Point-To-Point, Event-Driven Consumers, Task Executors, etc.

The original idea was build something pretty simple, **ErlBus** embodies [K.I.S.S](https://en.wikipedia.org/wiki/KISS_principle) principle. Because of this, things like [Durable Subscriber](http://www.enterpriseintegrationpatterns.com/DurableSubscription.html), [Store-And-Forward](http://www.enterpriseintegrationpatterns.com/Introduction.html), etc., are not supported here, but the good thing is that you can go adding more modules and functions on top of **ErlBus** in order to provide missing things like mentioned before, depending on your needs –it's a kind of backbone to build more complex systems, tools, etc.
