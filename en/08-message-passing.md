Message Passing
---------------

This is another topic that you have already gone over in depth in
previous chapters. As with everything else there are some tips and
tricks you can use to reduce the likelihood of errors and make future
changes easier. You have already seen the bare send examples the
previous parts of this book. Lets rehash though

```erlang

    Pid ! {some_message, SomeDataPackage}.

```

The problem with this approach is that you are exposing internal data
structures to your users. You are removing your ability to change this
API as needed without affecting your users. Fortunately, you can
rectify this by simply encapsulating your send logic in a
function. Then expose the function to your users. This will allow you
to change your API as much as you want without affecting your external
API. So our example above becomes.

```erlang

send_some_message(Pid, SomeDataPackage) ->
    Pid ! {some_message, SomeDataPackage}.

```

This approach is ubiquitous in throughout Erlang because its so
useful. This continues to be useful when you even after you start
using the OTP `gen_server` modules. In that case our function just
becomes.

```erlang

send_some_message(Pid, SomeDataPackage) ->
    gen_server:cast(Pid, {some_message, SomeDataPackage}).

```

I encourage you to get into the habit of defining your APIs in this
way and hiding the structure and the format of the messages. In the
long run it will save you a huge amount of pain and effort.

As you progress in your understanding of Erlang you will use bare
sends (the '!') less and less. It will pretty quickly get replaced
with use of `gen_server` and its various call/cast functions. There
will be times though when you will make a conscious decision to use
bare messages and you need to understand how you can protect yourself
from message capture.

By message capture I mean inadvertently processing messages that you
do not intend to process. This is actually much easier then you would
expect. Lets say that you have a receive clause that looks something
like this.

```erlang

receive
    DataIWant ->
        process_my_data(DataIWant)
end

```

This receive clause is going to capture and process any message that
comes into the process mailbox. This is true whether or not you
actually want to process that message. The right way to handle this is
to tag your messages in such a way that you can match against the
tag. This is also where encapsulating your sends comes in handy. If we
change the receive clause here just slightly we can remove this
problem.

```erlang

receive
    {my_data, DataIWant} ->
        process_my_data(DataIWant)
end

```

This allows us to receive only those messages that we are actually
interested in receiving.
