Anonymous Functions
-------------------

In our last section we learned some tips and tricks for efficiently
using tail recursion. The truth is that we don't often need to do this
type of iteration ourselves. In many cases, we can make use of
existing functionality provided in the various applications
distributed with Erlang. We do this by making use of anonymous
functions. Anonymous functions are something that you will find really
useful as you continue your course of study here.

Lets take our 'sums' function above and refactor it a bit into an
anonymous function and use some of the built in goodness of Erlang's
standard library to iterate over a list of numbers.

```erlang

sum_with_iter(SumList) ->
    Fun = fun(Elem, Acc) ->
              Elem + Acc
          end,

    lists:foldl(Fun, 0, SumList).

```

As you can see the accumulator approach to tail recursion and
iteration is built right into the modules distributed with Erlang. In
this case the 'lists:foldl' function makes use of accumulators to make
iteration easier. So we create an anonymous function that takes two
parameters. The element of the list to be processed and the
accumulator. We then just implement the business logic in the function
body. In this case its just adding the element to the accumulator. We
let the 'lists:foldl' function handle the the actual iteration for
us. It costs a bit of additional overhead for us, but it has the
tendency to be more clear.

Of course, if we could just cheat a little and use the sum function
from the lists module. The fact that a 'sum' function exists in the
lists module is just one of those little idiosyncrasies that you find
in Erlang from time to time.

Lets do another example. We can take our `filter_in_range` function
above and rework it a bit to use the iteration constructs available to
us in the lists module.

```erlang

filter_numbers_in_range(FilterList) ->
    Fun = fun(Elem) when Elem > 0 andalso Elem < 100 ->
              true;
          (_) ->
              false
          end,
    lists:filter(Fun, FilterList).

```

This reimplementation `filter_numbers_in_range` is very
similar to our reimplementation of the sums function. That's because
the approach is very similar, take the business logic and encapsulate
it in a function. Then let the functions distributed with the system
handle the actual iteration. This should make your system more
maintainable and generally understandable.

Occasionally, our business logic gets pretty long. When that's the
case we may want to put it in its own named function. We can then
create an Anonymous Function from the named function using a little
bit of syntactic magic. Lets re-implement our filter using these
function references.

```erlang

is_number_between_0_and_100(Num) when Num > 0 andalso < 100 ->
    true;
is_number_between_0_and_100(Num) ->
    false.


filter_numbers_in_range(FilterList) ->
    Fun = fun is_number_between_0_and_100/1,
    lists:filter(Fun, FilterList).

```

In this case we implemented a named function just for our filter. We
then just grabbed the named function using the 'fun'
operator. Remember that in Erlang functions are identified by their
name and their arity, so we passed \verb|<function_name>/<arity>| to
the fun operator. We can do the same for functions in other modules by
extending the name a bit. In that case we just do
`<module>:<function_name>/<arity>`. Note the colon between the
module name and the function name. This is useful when the
functionality that you want to make use of is already defined in some
other module. Why wrap it or redefine it when you can just refer to
it.

For example, lets say we had defined our filter function in a module
called foo. Then our `filter_numbers_in_range` function would have
been changed in the following manner.

```erlang

filter_numbers_in_range(FilterList) ->
    Fun = fun foo:is_number_between_0_and_100/1,
    lists:filter(Fun, FilterList).

```

You can, of course, do this with any function that you have access to.
