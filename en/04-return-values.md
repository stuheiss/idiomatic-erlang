Return Values
-------------

Return values is an area where there is a bit of controversy in the
community. Originally, most code was written to return a status
evaluation and a value. This was somewhat akin to C code that uses the
return value to return the error state while using pointers in
parameters to return the result of evaluation.  In Erlang, we don't
have pointers but we do have tuples, so we return the status and the
result as a tuple. However, since the introduction of exceptions the
the dominant idiom is emergint as the use of exceptions to indicate
status and values being transfered through the return.

To demonstrate the difference between the old and new styles we go
back to the binary tree lookup. You may have noticed that the lookup
function was missing some important features for a real
implementation. At the moment it only takes into account instances
where the key searched for exists. It doesn't take into account
failure cases at all. Lets fix that by returning a status/value.

```erlang

lookup(Key, {Key, Val, Left, Right}) ->
    {ok, Val};
lookup(Key, {Key1, _, none, _}) when Key > Key1 ->
    undefined;
lookup(Key, {Key1, _, _, none}) when Key > Key1 ->
    undefined;
lookup(Key, {Key1, Val, Left, Right}) when Key < Key1 ->
    lookup(Key, Left);
lookup(Key, {Key1, Val, Left, Right}) when Key > Key1 ->
    lookup(Key, Right).

```

Now it will return `{ok, Val}` when successful and the
bare atom `undefined` when not. You can even insert a key with a value
of 'undefined' without any ambiguity because all return values are
wrapped in the `{ok, _}` tuple while keys with new values
get the bare 'undefined'.

Unfortunately, checking return values for every function we call can
be a bit tedious, even with all this wonderful pattern matching. We
have the ability produce a better result by using exceptions to
indicate failure cases.

```erlang

lookup(Key, {Key, Val, Left, Right}) ->
     Val;
 lookup(Key, {Key1, _, none, _}) when Key > Key1 ->
     throw(undefined_key);
 lookup(Key, {Key1, _, _, none}) when Key > Key1 ->
     throw(undefined_key);
 lookup(Key, {Key1, Val, Left, Right}) when Key < Key1 ->
     lookup(Key, Left);
 lookup(Key, {Key1, Val, Left, Right}) when Key > Key1 ->
     lookup(Key, Right).

```

In this case the you can accept the return value and go on your merry
way. However, if you want to do something special in the case of a
missing key you can handle that with the usual exception handling
routines.

We can actualy make this even more simple by truly following the
spirit of Erlang and only coding for the correct case. The easiest and
in some cases the most correct thing to do is to do nothing as in the
first example of the lookup function. In this case the
`function_clause` exception will be generated automatically. The
drawback here is that it may not always be clear what cuased the
exception so as to be able to differentiate one failure from
another. In cases where differentiation is required generating a
custom exception is best.

You will find error and exception style code mixed pretty freely. This
is even the case in the OTP applications distributed with Erlang. So
you should be familiar with both styles. You are encouraged to use the
exception throwing style however.
