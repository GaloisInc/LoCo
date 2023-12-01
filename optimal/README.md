# `Optimal(L)`

`Optimal(L)` is an embedded DSL that allows for automated lazy evaluation and
caching of expressions in a particular source language. It is written
`Optimal(L)` to suggest a function application, with `Optimal` being the function
and `L` being the parameter. This is to illustrate that `Optimal` is parametric
over the expression language, `L`, that it harnesses. In principle, `Optimal`
can be instantiated with any number of expression languages, though at the
moment, the only supported language is Haskell.

`Optimal(L)` works by binding expressions to identifiers, and allowing reuse of
those identifiers in subsequent expressions. Behind the scenes, it works to
ensure that a reference to another `Optimal`-bound expression either reuses a
prior evaluation of that expression, or populates a cache to allow future reuse
of the expression's result. In this way, `Optimal` has a semantics that is both
lazy and ensures an "evaluated at most once" principle.

For more details, see our [reference guide](./doc/reference.md).


## Future Work

One future line of work could be reworking the compilation process to produce
data dependency graphs, or generating them as computations are performed. There
exists limited support for real-time "tracing" of binds as they're computed.

Another is more principled support for tuple patterns. The support is currently
limited to non-nested 2-tuples and 3-tuples.

More research on commutative monads and how `Optimal` can take advantage of them
would probably benefit the tool, and would provide more motivating use cases.
