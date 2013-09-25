Pattern Matching
----------------

### Conditional Dispatch

Erlang has a first class set of Conditionals in `if` and `case`
expressions. Like almost every other type of branching in Erlang these
conditionals use patter matching as well.  This use of pattern
matching in the language means that these conditionals are very
flexible and powerful. However, you wont see them used very often at
all. In most cases conditional pattern matching happens in function
heads instead of conditionals within a function clause. The main reasons
for this are ease readability and maintainability.

Examples are always the best way to get your mind around these
concepts so lets start out with a one. So lets start with a snippet of
binary tree search code that illustrates this.

```erlang

lookup(Key, {Key, Val, Left, Right}) ->
   Val;
lookup(Key, {Key1, Val, Left, Right}) when Key < Key1 ->
   lookup(Key, Left);
lookup(Key, {Key1,Val,Left,Right}) when Key> Key1 ->
   lookup(Key, Right).

```

Here all branching logic is contained within the function heads
themselves. The function bodies only handle the actual logic that
needs to be executed on that branch. This is considered more concise
and readable then the alternative format using conditionals.

For the next example the tree lookup function will be augmented with
additional code to convert any integer values found into strings for
printing. This will be written using the case conditional construct.

```erlang

lookup(Key, Value) ->
    case Value of
      {Key, Val, Left, Right} ->
         case Val of
            Val when is_string(Val) ->
               Val;
            Val when is_integer(Val)
               integer_to_list(Val)
         end;
      {Key1, Val, Left, Right} when Key < Key1 ->
          lookup(Key, Left);
      {Key1,Val,Left,Right} when Key > Key1 ->
          lookup(Key, Right)
    end.

```

Here one large function with conditional expressions is used to do the
work desired. This is a very imperative style of programming. In
Erlang seek to use function heads to handle conditional logic. This
practice has the additional implication that you will use more
functions and smaller functions. This style provides for not only more
readability but more intentionality. It becomes more clear what each
function and each section of code is intended to do. The next example
features the code we just saw rewritten with this idiom in mind.

```erlang

lookup(Key, {Key, Val, Left, Right}) ->
   convert_if_integer(Val);
lookup(Key, {Key1, Val, Left, Right}) when Key < Key1 ->
   lookup(Key, Left);
lookup(Key, {Key1,Val,Left,Right}) when Key> Key1 ->
   lookup(Key, Right).

convert_if_integer(Val) when is_string(Val) -> Val;
convert_if_integer(Val) when is_integer(Val) -> integer_to_list(Val).

```

This, somewhat complex, decision tree gets broken down into a pretty
easily digest able set of small functions with a few clauses each.
