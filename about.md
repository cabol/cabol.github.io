---
layout: page
permalink: /about/index.html
title: Carlos Andres Bolaños R.A
imagefeature: cabra_photo.jpg
chart: true
---
<figure>
  <img src="{{ site.url }}/images/cabra_photo.jpg" alt="Carlos Andres Bolaños R.A">
  <figcaption>Carlos Andres Bolaños R.A</figcaption>
</figure>

{% assign total_words = 0 %}
{% assign total_readtime = 0 %}
{% assign featuredcount = 0 %}
{% assign statuscount = 0 %}

{% for post in site.posts %}
    {% assign post_words = post.content | strip_html | number_of_words %}
    {% assign readtime = post_words | append: '.0' | divided_by:200 %}
    {% assign total_words = total_words | plus: post_words %}
    {% assign total_readtime = total_readtime | plus: readtime %}
    {% if post.featured %}
    {% assign featuredcount = featuredcount | plus: 1 %}
    {% endif %}
{% endfor %}


Software Engineer with a focus on distributed systems development, HA systems and highly scalable Internet-based architectures. More than 10 years of experience working for different industries such as: Telecommunications, E-Commerce, Banking, Retail, B2C, etc.</p>

Specialties: Distributed systems programming, Erlang/Elixir, C/C++, Java, C#, UNIX/POSIX programming.
