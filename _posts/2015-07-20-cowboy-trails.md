---
layout: post
title: "Cowboy Trails"
date: 2015-07-20 13:00
comments: true
categories: inaka erlang cowboy-trails
author: Carlos Andres Bolaños
ispost: true
abstract: "Cowboy routes on steroids!"
feature_image: "https://unsplash.it/1200/400?image=379"
---

Today the most used web server and framework to build web-based applications in Erlang world is [Cowboy](https://github.com/ninenines/cowboy), which is awesome, but it doesn't mean that it cannot be improved to make it even better. Furthermore, because Cowboy is an Open Source project, it is pretty easy to extend it and here is where [cowboy-trails](https://github.com/inaka/cowboy-trails) comes in.
<!--more-->

First, let's start explaining why we use `cowboy-trails`:

1. To add extra information to Cowboy routes, which can be used later to interact with the server in a higher abstraction level.

Suppose that you want to add additional information (metadata) to cowboy routes related to the semantics of each HTTP method:

{% highlight erlang %}
Metadata = #{put  => #{description => "PUT method"},
             post => #{description => "POST method"},
             get  => #{description => "GET method"}},
Trail = trails:trail("/",
                     cowboy_static,
                     {private_file, "index.html"},
                     Metadata,
                     []),
%% You can later retrieve the metadata:
Metadata = trails:metadata(Trail),
{% endhighlight %}

2. We wanted to make it easier to compile Cowboy routes, for example: there is a common case when you work with Cowboy in which you normally compile all routes under a single host. In this case, `cowboy-trails` provides a simple function to make it easier `trails:single_host_compile/1`.

{% highlight erlang %}
%% You only define the routes/paths
Routes = [ {"/resource1", resource1_handler, []}
         , {"/resource2/[:id]", resource2_handler, []}
         ],
trails:single_host_compile(Routes),
{% endhighlight %}

3. We also wanted to allow you to define routes on each handler instead of a single place. This could be one of the most important reasons, because it helps you organize your code better, since each handler will be responsable for its own routes.

Normally, when you work with `cowboy` you have to define all routes in one place, for example, like this:

{% highlight erlang %}
Routes =
  [{'_',
    [ {"/", cowboy_static, {file, "www/index.html"}}
    , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
    , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
    , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
    , {"/api/status", spts_status_handler,  []}
    , {"/api/games", spts_games_handler, []}
    , {"/api/games/:game_id", spts_single_game_handler, []}
    , {"/api/games/:game_id/serpents", spts_serpents_handler, []}
    , { "/api/games/:game_id/serpents/:token"
      , spts_single_serpent_handler, []
      }
    , {"/api/games/:game_id/news", lasse_handler, [spts_news_handler]}
    ]
   }
  ],
Dispatch = cowboy_router:compile(Routes),
{% endhighlight %}

With `trails` you're able to define the routes on each resource handler, and your handler must implement the callback `trails/0` and return the specific routes for that handler. Once you have implemented the `trails/0` callback on your handlers, you can do something like this:

{% highlight erlang %}
Handlers =
  [ spts_status_handler
  , spts_games_handler
  , spts_single_game_handler
  , spts_serpents_handler
  , spts_single_serpent_handler
  , spts_news_handler
  ],
Trails =
  [ {"/", cowboy_static, {file, "www/index.html"}}
  , {"/favicon.ico", cowboy_static, {file, "www/assets/favicon.ico"}}
  , {"/assets/[...]", cowboy_static, {dir, "www/assets"}}
  , {"/game/:game_id", cowboy_static, {file, "www/game.html"}}
  | trails:trails(Handlers)
  ],
trails:single_host_compile(Trails),
{% endhighlight %}

Doing it this way, each handler keeps its own routes, as it should, allowing you then to merge them easily.

4. We also added other helper functions such as:

`cowboy` routes storage:

{% highlight erlang %}
%% Define a set of rules
Trails =
  [ {"/resource1/[:id]", trails_test_handler1, []}
  , {"/resource2/test", [], trails_test_handler2, [arg0]}
  , {"/resource3", [], trails_test_handler3, []}
  ],
%% Store those routes
ok = trails:store(Trails),
%% Get all stored routes
Trails = trails:all(),
%% And retrieve specific routes using the path as key
#{path_match := "/resource1/[:id]"} = trails:retrieve("/resource1/[:id]"),
{% endhighlight %}

Finally, to learn more about `cowboy-trails` please check the [Project](https://github.com/inaka/cowboy-trails) and [Example](https://github.com/inaka/cowboy-trails/tree/master/example).
