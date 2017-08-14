---
layout: post
title: "Introducing Nebulex, a fast, flexible and powerful caching library for Elixir"
description: "This post is an introduction to Nebulex, a local and distributed caching tool for Elixir."
date: 2017-08-14
categories: posts
tags: [Elixir, Erlang, Cache, Distributed Cache, Sharding]
comments: true
feature_image: "https://unsplash.it/1200/400?image=537"
---

Caching might be one of the most common and used techniques to improve
performance, and in Elixir there are different [options available](https://github.com/h4cc/awesome-elixir#caching);
some of them very good options. However, most of them are focused on local
caching, but let's face it, we seldom deploy our systems in a single node, it's
not a common scenario, specially in the Elixir/Erlang world, is it? Most of the
cases we deal with distributed systems, which involves at least two nodes,
therefore, sometimes a local cache might be not enough, for those cases we might
also need a distributed cache, that give us not only good performance but also
linear scalability, and of course, make data accessible from any node of the
cluster. But you might be wondering, why not both? Why not a tool or library
that give us the ability to create different caching topologies depending on
our needs; it could be a simple local cache, a distributed or partitioned cache,
or even a near cache topology. Well, this is where [Nebulex](https://github.com/cabol/nebulex) comes in!
<!--more-->

[Nebulex](https://github.com/cabol/nebulex) is an open source caching library
written in Elixir, highly inspired by [Ecto](https://github.com/elixir-ecto/ecto),
taking advantage of many of its benefits. **Nebulex** brings new powerful and
useful features to the table, these are some of them:

  * Simple and fluent API inspired by [Ecto](https://github.com/elixir-ecto/ecto)
  * Flexible and pluggable architecture like Ecto – based on [adapter pattern](https://en.wikipedia.org/wiki/Adapter_pattern)
  * Built-in cache adapters
    - [Local generational Cache](http://hexdocs.pm/nebulex/Nebulex.Adapters.Local.html)
    - [Distributed or Partitioned Cache](http://hexdocs.pm/nebulex/Nebulex.Adapters.Dist.html)
    - [Multi-level Cache](http://hexdocs.pm/nebulex/Nebulex.Adapters.Multilevel.html)
  * Support for different cache topologies – [Partitioned, Near, etc.](https://es.slideshare.net/C0deKhan/distributed-caching-essential-lessons-ts-1402)
  * Time-based expiration
  * Pre/post execution hooks
  * Transactions (key-locking)
  * Key versioning – [optimistic offline locks](https://martinfowler.com/eaaCatalog/optimisticOfflineLock.html)
  * Optional statistics gathering

Besides, **Nebulex** is very well documented, so the idea is you start
checking out the next links and confirm by yourself how that easy it is,
and of course, all mentioned features above:

* [Online Documentation](http://hexdocs.pm/nebulex/Nebulex.html)
* [Getting Started Guide](http://hexdocs.pm/nebulex/getting-started.html)
* [Examples](https://github.com/cabol/nebulex_examples)
  - [Partitioned Cache](https://github.com/cabol/nebulex_examples/tree/master/partitioned_cache)
  - [Near Cache](https://github.com/cabol/nebulex_examples/tree/master/near_cache)
  - [Nebulex and Ecto Integration](https://github.com/cabol/nebulex_examples/tree/master/nebulex_ecto_example)
  - [Ecto Fallback](https://github.com/cabol/nebulex_examples/tree/master/ecto_fallback)
  - [Using Nebulex from Erlang](https://github.com/cabol/nebulex_examples/tree/master/erlang_cache)

Finally, if you find any bug or you have a suggestion, please feel free to
[open an issue](https://github.com/cabol/nebulex/issues), besides, don't
hesitate to send us pull requests in case you want to contribute :)
