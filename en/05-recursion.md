Recursion
---------

We have already talked about tail recursion in previous chapters. So I
wont rehash the basic concept here. There are a few techniques,
though, that are commonly used in Erlang programs that make recursion
a little more tractable and efficient. Lets look at a naive example of
a simple function that iterates over a list and sums all the numbers
in the list.

```erlang

 sums([H | T]) ->
    H + sums(T);
 sums([]) ->
    0.

```

From you previous reading you should be able to see what the problem
right off the bat. The recursive call is not the last executable
statement. Here we are waiting for the function to return before we
can finish our statement. The stack frames are going to pile up and,
for very long lists, we may run out of space in memory for all those
useless stack frames.

There is a pretty easy fix for this. We just add something called an
accumulator. It allows us to carry processing forward into our next
function call. By using the accumulator we can do actual tail
recursion while still producing a viable result.

Lets change the function to use the accumulator we have been talking
about.

```erlang

sums([H | T], Acc) ->
    sums(T, H + Acc);
sums([], Acc) ->
    Acc.

```

The change is pretty subtle but important. We have added a new
parameter called 'Acc'. We are going to store our calculation there so
that it is available to the next iteration. In the body of our
function we just add the element we are processing, in this case 'H',
to our accumulator. When our list is empty, in the second clause, we
just return the accumulator. The accumulator contains the result of
our processing of the list.

This approach allows us to do properly tail recursive functions that
are capable of returning processing for the entire list. This is very
important if you want to iterate over objects of an arbitrarily large
size.

Lets do another example. In this case lets filter a list of numbers
for numbers in a specific range. You wont have to do this very often
and there are built in functions to make it much easier but it will
serve as a good, slightly more complex, example. We will start with
the naive example first, to show you how not to do it.

```erlang

filter_numbers_in_range([H | T])
     when H > 0 andalso H < 100 ->
     [H] ++ filter_numbers_in_range(T);
filter_numbers_in_range([H | T]) ->
     filter_numbers_in_range(T);
filter_numbers_in_range([]) ->
     [].

```

We have a number of problems here. First and foremost our function
isn't tail recursive at all. Next we are doing an expensive operation,
the list append '++', for each element in the list. We can apply the
same approach we did for the sums to this function and solve many of
these problems.

```erlang

filter_numbers_in_range([H | T], Acc)
   when H > 0 andalso H < 100 ->
   filter_numbers_in_range(T, [H | Acc]);
filter_numbers_in_range([H | T], Acc) ->
   filter_numbers_in_range(T, Acc);
filter_numbers_in_range([], Acc) ->
   Acc.

```

As you can see we have added the accumulator parameters and used it to
build of the list of results. In the first function clause, where H is
in our range, we add the element to the front of our accumulator list
and do our tail call. In the second clause, where the H isn't in our
range, we just do our tail call without adding anything to the
accumulator. We essentially drop the value because it does not meet
our requirements. At the end we return the accumulator, which is the
result of our processing.

If you look closely there is still a minor problem here. Its pretty
subtle. When we add to the accumulator it prepends the element to the
front of the accumulator list. This has the effect of reversing the
direction values in the accumulator. Lets say we made the following
call.

```erlang

    > filter_numbers_in_range([51, -1, 99, 33, 200, 500, 1], []).
      [1, 33, 99, 51]
```

The list would come back in the opposite order from the original
argument. This has the potential to be confusing to our users. At the
very least, its non-intuitive. We can fix this pretty easily though.

```erlang

filter_numbers_in_range([H | T], Acc)
    when H > 0 andalso H < 100 ->
    filter_numbers_in_range(T, [H | Acc]);
filter_numbers_in_range([H | T], Acc) ->
    filter_numbers_in_range(T, Acc);
filter_numbers_in_range([], Acc) ->
    :reverse(Acc).

```

We made a pretty minor change here. At the end of processing we call
the lists:reverse function from the Stdlib application. It has the
effect of reversing the list. This adds a bit of overhead but makes
our result match our users expectation.

We have one last problem with the use of accumulators that we really
need to solve. The use of accumulators help us handle our requirements
in minimal space and time. However, its not really something we want
to expose to our users. It is of no benefit to them and, from their
prospective, only adds to the complexity of the interface. The
community has come up with a solution to this problem. We simply need
to front our functions that contain accumulators with functions that
do not and then export those functions.

```erlang

filter_numbers_in_range([H | T], Acc)
    when H > 0 andalso H < 100 ->
    filter_numbers_in_range(T, [H | Acc]);
filter_numbers_in_range([H | T], Acc) ->
    filter_numbers_in_range(T, Acc);
filter_numbers_in_range([], Acc) ->
    lists:reverse(Acc).

sums(SumList) ->
    sums(SumList, []).

sums([H | T], Acc) ->
    sums(T, H + Acc);
sums([], Acc) ->
    Acc.

```

We have added two new functions here that serve to make our API a
little more clear and comprehensive to our users. These functions hide
the accumulators and provide a clean API.
