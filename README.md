# Pattern Matching Example #

Only contains a typechecker.
Equipped with _exhaustivity checking_, meaning that it will check that all cases in a pattern match collectively cover everything that an input can be.
For example, if constructors `foo` and `bar` are defined, it will trigger an error if there is only a case for `foo`; the input _could_ be a `bar`, and we treat this as a compile-time error instead of a runtime error.

Also will give errors if certain patterns can never be hit.
Using the same definitions as before, this can happen if there are two `foo` cases, in addition to a `bar` case.

## Syntax ##

```
uname is a user-defined data type name
cname is a user-defined constructor name
x is a variable
i is an integer

type ::= `int` | uname
cdef ::= cname(type*)
tdef ::= `type` uname `=` cdef+
pattern ::= x | cname(pattern+)
case ::= `case` pattern `=>` exp
exp ::= i | x |
        | `let` x `=` exp `in` exp // variable introduction
        | cname(exp+) // constructor call
        | exp `match` { case+ } // pattern matching
```

## Running ##

You will need [`sbt`](https://www.scala-sbt.org/) installed.

```console
sbt
sbt> test
```
