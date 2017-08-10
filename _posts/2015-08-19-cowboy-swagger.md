---
layout: post
title: "Cowboy Swagger"
date: 2015-08-19 13:00
comments: true
categories: erlang cowboy rest documentation swagger cowboy-swagger
author: Carlos Andres Bolaños
ispost: true
abstract: "Swagger integration for Cowboy"
feature_image: "https://unsplash.it/1200/400?image=972"
---

In a previous post about [Swagger in Erlang](http://inaka.net/blog/2015/06/23/erlang-swagger-2015/), we had seen how to add a nice documentation to our RESTful APIs with Swagger, but following the long/tedious path:

- Download [Swagger-UI](https://github.com/swagger-api/swagger-ui/tree/master/dist) and put it within your `priv/swagger` folder.
- Add the new static contents to your Cowboy routes to be compiled.
- Then, create the `swagger.json` file (manually) which the Swagger UI feeds on to create the pages, and put it within `priv/swagger` folder.

Well, forget about all that. [Cowboy Swagger](https://github.com/inaka/cowboy-swagger) is an open source project built on top of [Trails](https://github.com/inaka/cowboy-trails) that integrates [Swagger](http://swagger.io), so you can have a really nice Web documentation for your RESTful APIs in only a few simple steps.

Now, let's see how simple and fast it is.

### 1. Document each Cowboy Handler

Because `cowboy_swagger` runs on top of `trails`, the first thing that you have to do is document all about your handler within the trails metadata. Keep in mind that all fields defined within each method into the metadata must be compliant with the [Swagger specification](http://swagger.io/specification).

For example, suppose that you have `example_echo_handler`, so it must implement the `trails/0` callback from `trails_handler` behaviour:

{% highlight erlang %}
trails() ->
  Metadata =
    #{get =>
      #{tags => ["echo"],
        description => "Gets echo var",
        produces => ["text/plain"]
      },
      put =>
      #{tags => ["echo"],
        description => "Sets echo var",
        produces => ["text/plain"],
        parameters => [
          #{name => <<"echo">>,
            description => <<"Echo message">>,
            in => <<"path">>,
            required => false,
            type => <<"string">>}
        ]
      }
    },
  [trails:trail("/message/[:echo]",
                example_echo_handler,
                [], Metadata)].
{% endhighlight %}

### 2. Include cowboy_swagger in your app

First, you need to include `cowboy_swagger_handler` module in your list of trails to be compiled.

{% highlight erlang %}
% Include cowboy_swagger_handler in the trails list
Trails =
  trails:trails([example_echo_handler,
                 example_description_handler,
                 cowboy_swagger_handler]),
% store them
trails:store(Trails),
% and then compile them
Dispatch = trails:single_host_compile(Trails),
{% endhighlight %}

The snippet of code above is usually placed when you start `cowboy`.

Then add `cowboy_swagger` to the list of apps to be loaded in your `*.app.src` file.

{% highlight erlang %}
{application, example,
 [
  {description, "Cowboy Swagger Example."},
  {vsn, "0.1"},
  {applications,
   [kernel,
    stdlib,
    jiffy,
    cowboy,
    trails,
    cowboy_swagger
   ]},
  {modules, []},
  {mod, {example, []}},
  {registered, []},
  {start_phases, [{start_trails_http, []}]}
 ]
}.
{% endhighlight %}

And that's it, you got it. Now start your application and then you will have access to the API docs under the path `/api-docs`. Supposing that you're running the app on `localhost:8080`, that will be [http://localhost:8080/api-docs](http://localhost:8080/api-docs).

Finally, you're welcome to check `cowboy_swagger` [sources](https://github.com/inaka/cowboy-swagger) and a full demonstrative example [here](https://github.com/inaka/cowboy-swagger/tree/master/example).
