Error Handling
--------------

### Let it crash

This is one of the most unique idioms in Erlang/OTP.  It is really a
hallmark of the language and one of the things that new programmers have
the the hardest time aclimating themselves to. In Erlang you should
try to code for the correct case, and only the correct case.

With Erlang/OTP programming if something goes wrong a standard
exception will be generated such as `bad_match` or `function_clause`
etc... These exceptions, if uncaught, will crash the process from
which they were generated.  Good OTP code uses supervisors which catch
and restart proccesses when they fail. This is much the same as what
Windows 95 users used to do on a regular basis with their machines;
when something went wrong they would reboot the machine to return it
to a known good state. This is actually a very effective way to get
out of a bad state. The problem is that the restart is not granular
enough; the whole system must come down which takes quite a bit of
time. Through the combination of processes and the supervisor
behaviour Erlang/OTP gives the programmer the ability to control
restarts at a very granular level. The whole system need not be
restarted in the presense of an error only one small part of it.

What this provides for you the programmer is that instead of a large
quantity of code littered with error handling you can write smaller
and consequently less error prone modules while deferring the error
handling to process restarts and supervision.

### Error check only the borders

In the ``let it crash'' section we talked about how you should code
for the correct case and defer error handling to processes and
supervision. To properly execute the correct case the data we pass in
to that code must be correct, this is obvious. In Erlang/OTP programs
we expect this to be the case, which is a start contrast to the
approach taken by many other languages. The way we do this is by
checking our borders. Error checking and data validation should be
done where your code touches the world outside itself - whatever you define
that to be. It can vary some in different situations. Once you verify
that the data expected is valid then the rest of your code should be
generally free of error checking. If something does happen to go
wrong supervision will take care of it.

In general Erlang/OTP programming should give you a loose and easy
feeling with regard to errors. Erlang programs are designed to be
robust through the use of OTP. A few errors here and there will not
take your system down so relax and think about the problem you are
trying to solve and not all of the possible things that could occur.
