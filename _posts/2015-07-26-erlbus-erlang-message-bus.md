---
layout: post
title: "ErlBus: Erlang Message Bus"
date: 2015-07-26
tags: [Erlang, Messaging, Programming]
image:
  feature: abstract-2.jpg
---

Messaging in [Erlang](http://www.erlang.org) is easy by default, because of its nature. But, why not make it even easier? ridiculously easy, and fun. Well, this is where [ErlBus](https://github.com/cabol/erlbus) comes in.

Let's start doing an implementation of a simple but very powerful messaging pattern: [Publish/Subscribe](http://www.enterpriseintegrationpatterns.com/PublishSubscribeChannel.html). In Erlang there are different ways to do it, but I'm going to mention only two (the common ones):

1. Using the native tool/library [pg2](http://erlang.org/doc/man/pg2.html).
2. Using external tool/library like [gproc](https://github.com/uwiger/gproc).

###Pub/Sub with pg2

**PG2** doesn't provides explicit Pub/Sub functions, so we have to implement ours, using the given primitives.

{% highlight erlang %}

sub(Channel, Handler) ->
  ok = pg2:create(Channel),
  ok = pg2:join(Channel, Handler).

{% endhighlight %}

---

more ...


THE END
