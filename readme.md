# What

Scala 3 macro-based library to deconstruct match/case expressions and return 
their structure for the further analysis. This was an experiment.

# Why

The original motivation was to write rules for a system inspired by
[Constraint Handling Rules](https://en.wikipedia.org/wiki/Constraint_Handling_Rules)
in the Scala syntax (that is, as embedded DSL).

Imagine we wanted a rule like this:

> If sequence of terms contains subsequence ["BEGIN", x, y, "END"] then rewrite this 
> subsequence as [x, "AND", y].

We wanted this to be represented like this:

```
case Contains("BEGIN", x, y, "END") => ReplaceWith(x, "AND", y)
```

Normal Scala pattern matching could not support this, hence the macro.

# What this macro does

Macro receives match expression and returns individual case clauses
(as individual partial functions) together with meta information about
patterns used in these individual case clauses. 

The resulting structure could be used to actually apply rules as appropriate.

See tests.

# Notes

Until we get good docs, this is helpful:

[QuotesImpl.scala](https://github.com/lampepfl/dotty/blob/246b60221da69682a2fbf815d23a3c08f7f40af0/compiler/src/scala/quoted/runtime/impl/QuotesImpl.scala)
