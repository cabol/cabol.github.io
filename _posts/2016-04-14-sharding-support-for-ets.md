---
title: "Transparent and out-of-box Sharding support for ETS tables in Erlang/Elixir."
description: "Sharding implementation on top of ETS tables, with the same ETS API."
date: 2016-04-14
categories: posts
tags: [Erlang, Elixir, Programming, ETS, Sharding]
comments: true
---

<img src="http://cabol.github.io/assets/posts/shards/steven_universe_gem_shards.png" align="right" style="float:right" height="420" width="380" />
This blog post is about how to scale-out [ETS](http://erlang.org/doc/man/ets.html) tables and be able to support high levels of concurrency without worrying about write-locks. Here is where [Shards](https://github.com/cabol/shards) comes in. [Shards](https://github.com/cabol/shards) is an **Erlang/Elixir** tool compatible with the ETS API, that implements [Sharding](https://en.wikipedia.org/wiki/Partition_(database)) support on top of ETS totally transparent and out-of-box.
<!--more-->

## Introduction

I'll start saying that [ETS](http://erlang.org/doc/man/ets.html) tables are just great, they work incredible well and it's pretty hard to beat them. To start suffering by locks issues (specially write-locks), it might require a huge traffic, probably millions of processes executing operations against a single ETS table, again, specially write operations – which involves locks. But for most of the cases, ETS is more than enough.

But, there might be scenarios where a single ETS table is not enough, probably scenarios with high levels of concurrency where write-locks becomes to a real problem, and you need a good way to scale-out your ETS tables. One of the most common architectural patterns to deal with this problem is [Sharding/Partitioning](https://en.wikipedia.org/wiki/Partition_(database)). For that reason, in many cases we're forced to implement **Sharding** on top of ETS tables ourselves, in order to balance load across multiple ETS tables instead of a single one, avoiding contention and being able to scale linearly – on-demand.

Because of that, it would be great to have a library/tool with the same ETS API, implementing [Sharding/Partitioning](https://en.wikipedia.org/wiki/Partition_(database)) on top of **ETS** totally transparent, with a very good performance/throughput and with the ability to scale linearly – go increasing number of shards as long as traffic increases to avoid suffering by locks and performance issues. Well, now it exist, let me introduce [Shards](https://github.com/cabol/shards), which is exactly the library/tool what we were talking about.

## Shards

**Shards** implements a layer on top of **ETS**, compatible with **ETS API** and adding **Sharding** support transparently.

When a table is created using `shards`, that logical table is mapped to `N` physical ETS tables, being `N` the number of shards – passed as argument when the table is created. Once the table shards are created, key-based operations like: `insert/2`, `lookup/2`, `delete/2`, etc., are distributed uniformly across all shards. Operations like `select`, `match`, `delete_all_objects/2`, etc., are implemented following a map/reduce pattern, since they have to run on all shards. But remember, you only have to worry about to use the API (which it's the same ETS API), the magic is performed by `shards` behind the scenes.

Now let's see what happens behind scenes when we create/delete a table using `shards`.

{% highlight erlang %}
% let's create a table, such as you would create it with ETS, with 4 shards
> shards:new(mytab1, [{n_shards, 4}]).
{mytab1,{4,#Fun<shards_local.pick_shard.3>,set}}
{% endhighlight %}

Exactly as ETS, `shards:new/2` function receives 2 arguments, the name of the table and
the options. With `shards` there are additional options:

 * `{n_shards, pos_integer()}`: allows to set the desired number of shards. By default, the number of shards is calculated from the total online schedulers.

 * `{scope, l | g}`: defines `shards` scope, in other words, if sharding will be applied locally (`l`) or global/distributed (`g`) – default is `l`.

 * `{pick_shard_fun, pick_shard_fun()}`: Function to pick the **shard** on which the `key` will be handled locally – used by `shards_local`. See the spec [HERE](https://github.com/cabol/shards/blob/master/src/shards_local.erl#L145).

 * `{pick_node_fun, pick_node_fun()}`: Function to pick the **node** on which the `key` will be handled globally/distributed – used by `shards_dist`. See the spec [HERE](https://github.com/cabol/shards/blob/master/src/shards_local.erl#L150).

 > **NOTE:** By default `shards` uses a built-in functions to pick the **shard** (local scope) and the **node** (distributed scope) on which the key will be handled. BUT you can override them and set your own functions, they are totally configurable by table, so you can have different tables with different pick-functions each.

Besides, the `shards:new/2` function returns a tuple of two elements:

{% highlight erlang %}
{mytab1, {4, #Fun<shards_local.pick_shard.3>, set}}
 ^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  1st                     2nd
{% endhighlight %}

The first element is the name of the created table; `mytab1`. And the second one is the
[State](./src/shards_local.erl#L159): `{4, #Fun<shards_local.pick_shard.3>, set}`.
We'll talk about the **State** later, and see how it can be used.

> **NOTE:** For more information about `shards:new/2` go [HERE](./src/shards_local.erl#L796).

Let's create another table:

{% highlight erlang %}
% create another one with default number of shards, which is the total of online
% schedulers; in my case is 8 (4 cores, 2 threads each).
% This value is calculated calling: erlang:system_info(schedulers_online)
> shards:new(mytab2, []).
{mytab2,{8,#Fun<shards_local.pick_shard.3>,set}}

% now open the observer so you can see what happened
> observer:start().
ok
{% endhighlight %}

If you open the `observer` app, you'll see something like this:

<p align="center"><a href="#">
  <img src="{{ site.baseurl }}/assets/posts/shards/shards_process_tree_1.png" height="250" width="350">
</a></p>

This is the process tree of `shards` application. When you create a new **"table"**, a new supervision tree is created.

1. Each logical table has its own supervision tree – dedicated only to that group of shards.
2. Each supervision tree has a main supervisor named with the same name of the table.
3. Each supervisor has a pool of workers (`gen_server`) – the pool size is the number of created shards.
4. Each worker or shard's owner, is responsible to create the assigned shard represented by an ETS table.
5. When you delete the table (`shards:delete/1`), the supervision tree is deleted, therefore, all created shards (ETS tables) are deleted automatically.

Let's delete a table:

{% highlight erlang %}
> shards:delete(mytab1).
true

> observer:start().
ok
{% endhighlight %}

See how `shards` gets shrinks:

<p align="center"><a href="#">
  <img src="{{ site.baseurl }}/assets/posts/shards/shards_process_tree_2.png" height="200" width="300">
</a></p>

**Shards** behaves elastically, more shards can be added/removed dynamically.

## Working with Shards

Now let's execute some read/write operations:

{% highlight erlang %}
% inserting some objects
> shards:insert(mytab1, [{k1, 1}, {k2, 2}, {k3, 3}]).
true

% let's check those objects
> shards:lookup(mytab1, k1).
[{k1,1}]
> shards:lookup(mytab1, k2).
[{k2,2}]
> shards:lookup(mytab1, k3).
[{k3,3}]
> shards:lookup(mytab1, k4).
[]

% delete an object and then check
> shards:delete(mytab1, k3).
true
> shards:lookup(mytab1, k3).
[]

% now let's find all stored objects using select
> shards:select(mytab1, ets:fun2ms(fun({K, V}) -> {K, V} end)).
[{k1,1},{k2,2}]
{% endhighlight %}

As you may have noticed, it's extremely easy, it's like use **ETS**, but using `shards` module instead of `ets`. Remember, almost all **ETS** functions are implemented by shards as well.

## Using shards_local directly

The module `shards` is a wrapper on top of two main modules:

 * `shards_local`: Implements **Sharding** on top of ETS tables, but locally (on a single Erlang node).
 * `shards_dist`: Implements **Sharding** but across multiple distributed Erlang nodes, which must run `shards` locally, since `shards_dist` uses `shards_local` internally. We'll cover the distributed part later.

When you use `shards` on top of `shards_local`, an extra call to the control ETS table owned by `shards_owner_sup` is done in order to recover the [State](https://github.com/cabol/shards/blob/master/src/shards_local.erl#L159), mentioned previously.
Most of the `shards_local` functions receives the **State** as parameter, so it must be fetched before
to call it.

If any microsecond matters to you, you can skip that call to the control ETS table by calling `shards_local` directly. Now the question is: how to get the **State**? Well, it's extremely easy, you can get the `state` when you call `shards:new/2` by first time, or you can call `shards:state/1` at any time you want, and then it might be store it within the calling process (which might be a `gen_server`), or wherever you want. E.g.:

{% highlight erlang %}
% take a look at the 2nd element of the returned tuple, that is the state
> shards:new(mytab, [{n_shards, 4}]).
{mytab,{4,#Fun<shards_local.pick_shard.3>,set}}
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

% State: {4, #Fun<shards_local.pick_shard.3>, set}
% 1st element is the number of shards
% 2nd element is the pick function to select the shard
% 3rd element is the type of ETS table

% you can also get the state at any time you want
> shards:state(mytab).
{4,#Fun<shards_local.pick_shard.3>,set}
{% endhighlight %}

Most of the cases this is not necessary, `shards` wrapper is more than enough, it adds only a
few microseconds of latency. In conclusion, **Shards** gives you the flexibility to do it,
but it's your call!

 > **NOTE:** You can see how small is the difference in terms of latency between `shards` and `shards_local` in the **Performance Tests** section below – it'll be covered later.

## Distributed Shards

In this section we'll see how `shards` works but distributed.

**1.** Let's start 3 Erlang consoles running shards:

Node `a`:

{% highlight text %}
$ erl -sname a@localhost -pa _build/default/lib/*/ebin -s shards
{% endhighlight %}

Node `b`:

{% highlight text %}
$ erl -sname b@localhost -pa _build/default/lib/*/ebin -s shards
{% endhighlight %}

Node `c`:

{% highlight text %}
$ erl -sname c@localhost -pa _build/default/lib/*/ebin -s shards
{% endhighlight %}

**2.** Create a table with global scope (`{scope, g}`) on each node:

{% highlight erlang %}
% when a tables is created with {scope, g}, the module shards_dist is used
% internally by shards
> shards:new(mytab, [{n_shards, 4}, {scope, g}]).
{mytab,{4,#Fun<shards_local.pick_shard.3>,set}}
{% endhighlight %}

**3.** Setup the `shards` cluster.

From node `a`, join `b` and `c` nodes:

{% highlight erlang %}
> shards:join(mytab, ['b@localhost', 'c@localhost']).
[a@localhost,b@localhost,c@localhost]
{% endhighlight %}

Let's check that all nodes have the same nodes running next function on each node:

{% highlight erlang %}
> shards:get_nodes(mytab).
[a@localhost,b@localhost,c@localhost]
{% endhighlight %}

**4.** Now **Shards** cluster is ready, let's do some basic operations:

From node `a`:

{% highlight erlang %}
> shards:insert(mytab, [{k1, 1}, {k2, 2}]).
true
{% endhighlight %}

From node `b`:

{% highlight erlang %}
> shards:insert(mytab, [{k3, 3}, {k4, 4}]).
true
{% endhighlight %}

From node `c`:

{% highlight erlang %}
> shards:insert(mytab, [{k5, 5}, {k6, 6}]).
true
{% endhighlight %}

Now, from any of previous nodes:

{% highlight erlang %}
> [shards:lookup_element(mytab, Key, 2) || Key <- [k1, k2, k3, k4, k5, k6]].
[1,2,3,4,5,6]
{% endhighlight %}

All nodes should return the same result.

Let's do some deletions, from any node:

{% highlight erlang %}
> shards:delete(mytab, k6).
true
{% endhighlight %}

And again, let's check it out from any node:

{% highlight erlang %}
% as you can see 'k6' was deleted
> shards:lookup(mytab, k6).
[]

% check remaining values
> [shards:lookup_element(mytab, Key, 2) || Key <- [k1, k2, k3, k4, k5]].    
[1,2,3,4,5]
{% endhighlight %}

> **NOTE**: This module is still under continuous development. So far, only few
  basic functions have been implemented.

## Performance Tests

There is no better way to prove that it really works and worth than with performance tests. Therefore, this section presents a set of tests that were made in order to show how `shards` behaves compared with `ets` – especially under heavy load.

Performance tests are not easy to perform, so one of the best recommendation is to find a good tool to help with that. In this case, tests were done using [basho_bench](https://github.com/basho/basho_bench) tool. I strongly recommend it, it's a very good tool, simple, easy to use, provides graphics, examples, etc. Read more about it: [Basho Bench](http://docs.basho.com/riak/latest/ops/building/benchmarking/).

 > **NOTE:** The original repository of **basho_bench** was forked in order to add **`shards`** driver. You can find it [HERE](https://github.com/cabol/basho_bench).

### Test Environment

 * Laptop MacBook Pro (OSX El Capitan)
 * Hardware: 4 cores, 16G of RAM, 256G SSD
 * Erlang OTP 18.2.1

### Latencies

In the next figure, we can notice how is the latency trend for each module. Latencies for `ets` tends to increase with the traffic faster than `shards`, which is precisely the goal, be able to scale across locks, adding more shards as long as traffic increases. In this case, the test was done using 4 shards, and you can see how with the generated traffic (~150.000 ops/sec), `shards` and `shards_local` latencies were better than `ets` – only a few microseconds but the thing is there was an improvement.

<p align="center"><a href="#">
  <img src="{{ site.baseurl }}/assets/posts/shards/Latencies_ETS_Vs_Shards_4shards.png" height="600" width="100%">
</a></p>
<p align="center"><span class="caption text-muted">Latencies: ets vs shards vs shards_local.</span></p>

### Throughput

The max generated throughput was ~150.000 ops/seg for all compared modules – oscillations were between 100.000 and 200.000 ops/sec.

<p align="center"><a href="#">
  <img src="{{ site.baseurl }}/assets/posts/shards/Throughput_ETS_Vs_Shards_4shards.png" height="600" width="100%">
</a></p>
<p align="center"><span class="caption text-muted">Throughput: ets vs shards vs shards_local.</span></p>

Unfortunately, due to Hardware limitations, it wasn't possible to do the tests at least with one million of ops/seg. However, any help is welcome and would be great!

## Summing Up

It's clear that Sharding is the way to go when we start suffer of locks and/or contention. And `shards` is the library/tool to help with that, in order to avoid implement Sharding over and over again on top of ETS tables ourselves.

For more information please visit the [Shards GitHub Repo](https://github.com/cabol/shards).
