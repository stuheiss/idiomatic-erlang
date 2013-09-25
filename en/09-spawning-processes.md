Spawning Processes
------------------

You have already learned how to spawn processes in previous
chapters. Those methods work just fine and you wont get into any
trouble using them. However, there is an easy way to spawn process
that will give you a lot more information if something goes wrong. You
do this by using the functions defined in the `proc_lib` module
of stdlib. There is a lot of functionality defined there but for now
we are just going to use the 'spawn' methods.

The example below shows starting a buggy process with the bare spawn
and with the `proc_lib` spawn.

```shell

             1> spawn(fun() -> io_lib:format("1") end).

             =ERROR REPORT==== 7-Jun-2008::20:20:49 ===
             Error in process <0.33.0> with exit value:
                {undef,[{io_lib,format,["1"]}]}
             2> application:start(sasl).

             3> proc_lib:spawn(fun() -> io_lib:format("1") end).

             =CRASH REPORT==== 7-Jun-2008::20:42:27 ===
               crasher:
                 pid: <0.54.0>
                 registered_name: []
                 exception error: undefined function io_lib:format/1
                 initial call: interpreted function with arity 0
                 ancestors: [<0.31.0>]
                 messages: []
                 links: []
                 dictionary: []
                 trap_exit: false
                 status: running
                 heap_size: 610
                 stack_size: 23
                 reductions: 157
               neighbours:

```

The `proc_lib` spawn contains quite a bit more information then that
produced by the spawn. Information about registered names, errors,
ancestors etc. A lot of this information wont be very useful to you
right now. As you progress through this book it will become more and
more comprehensible. For right now just try to get in the habit of
using `proc_lib:spawn` instead of the bare spawn. You will be giving
your future self a big leg up on debugging problems.

Of course, this information is only available to you if the sasl
application is started. Even if the sasl application isn't started it
works just fine, only without printing out this additional information
if the case of error.
