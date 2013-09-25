Types
-----

Erlang is a dynamically typed language so there isn't a way for you to
create new types specific to your application. For the most part this
isn't a problem. As with any dynamic language you assume that the data
passed into your function is correct and process it
accordingly. Incorrect data will cause an error. However, there are
times when you want to protect your data or you want to guard your
functions in such a way that you will only receive data you are
expecting. There is an idiom in Erlang that has evolved to make this
possible. It involves 'tagging' your data. You do this by wrapping
your data in a two element tuple where the first element is an atom
describing your data. We saw a use of this in our last section. The
{ok, Value} construct is nothing more then a tagged type. A piece of
data that carries its description with it. You can do this pretty
easily for your own types as well.

Lets build up a simple example. We will write a simple module to
handle association lists. We wont try to describe the complete
implementation but we will hit the API and the major points that
describe tagged types well.

```erlang

-module(assoc_list).

-export([new/0]).

new() ->
    {assoc_list, []}.

add(Key, Value, {assoc_list, Data}) ->
     ...

get(Key, {assoc_list, Data}) ->
     ...

delete(Key, {assoc_list, Data}) ->
     ...
```

There are a couple of things to notice here. First, because we are
creating our own type, we have a 'new' function. This allows us to tag
the value and hide the implementation from the user. That hiding of
the implementation allows us to change the representation when we need
to. This is an important feature for long term maintainability. Second
we wrap that data in a tuple where the first element in the tuple is
an atom describing data. In later functions we match on the tuple
representing our data. This guard ensures that only data we can
manipulate gets past the guards. It also ensures that this particular
problem is reported early.

Don't get to crazy with this approach. In general you wont have to do
this very often. Mainly when you want to define new generic structures
or you want to hide your implementation. In other cases its probably a
better idea to just use the built in types and assume correct data.
