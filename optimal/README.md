# DRAFT

# What is `Optimal(L)`?

`Optimal(L)` is an embedded DSL that allows for automated lazy evaluation and
caching of expressions in a particular source language. It is written
`Optimal(L)` to evoke a function application, with `Optimal` being the function
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


# How do I use `Optimal(L)`?

`Optimal(L)` is embedded in Haskell. This is true regardless of the choice of
`L` (`L`, recall, is also limited to being Haskell at the moment). Users of
`Optimal` import the library and write their `Optimal` code in a quasiquoted
block, using the `optimal` quoter.

Within this quoter, users can declare types:

```hs
[optimal|
type Foo = { x : Bool, y : Char }
|]
```

This says that a `Foo` is a "module" with fields `x` of type `Bool` and `y` of
type `Char`. A module is like a struct: it is a collection of values accessible
by their name.

Note that we have no definition in `Optimal` of `Bool` or `Char`. When
encountering "unbound" types like this, `Optimal` will try to generate code that
relies on a Haskell type with that name. Subsequent declarations referring to
`Foo`, though, will use our declared type.

TODO: type aliases

TODO: lists

With a module type declared, users can create actual modules of that type:

```hs
isPrime :: Int -> Bool
isPrime = undefined

[optimal|
foo : Foo
foo = {
  x = <| pure (isPrime 30000001) |>,
  y = <| pure (if x then 'T' else 'F') |>,
}
|]
```

This defines a module `foo` of type `Foo`. It populates the field named `x` with
whether or not a large(-ish) number is prime, and the field named `y` with a
computation depending on that primality test. (More on the mechanics of this
reference to `x` below. TODO.) The content within the `<| ... |>` delimiters
contains expressions in the language `L`, which is ordinary Haskell (which can
refer transparently to values defined outside the scope of an `Optimal`
quotation). Don't worry about the use of `pure` yet.

TODO: worry about `pure`

We've written the module by declaring `x` before declaring `y`, but this is not
necessary, even though `y` mentions `x`. A side effect of `Optimal`'s efforts to
share computation mean that declarations in generated Haskell code are
automatically topologically reordered.

TODO: well-typedness?

This generates a few declarations:
```hs
foo :: MonadIO m => m (Foo m)
x :: MonadIO m => Foo m -> Thunked m Bool
y :: MonadIO m => Foo m -> Thunked m Char
```

The Haskell type generated for `Foo` can be considered a variation on a built-in
Haskell record type, with `foo` as its introducer and `x` and `y` as its
eliminators. (Technically, `x` and `y` are not generated as functions, but
appear as record accessors, so their signatures cannot be included in a file.
TODO.) The extra monadic machinery and appearance of the `Thunked` type are
indications of the special semantics that `Optimal` imposes on this declaration.

Users need to know little about `Thunked` values, other than the type of their
eliminator:
```hs
force :: MonadIO m => Thunked m a -> m a
```

A `Thunked` value can be thought of as a suspended computation, and `force`
evaluates that computation to yield its result. TODO it does more.

With this in mind, we can write some client code for this module:
```hs
workWithFoo :: IO ()
workWithFoo =
  do
    f <- foo
    force (x f) >>= print
    force (y f) >>= print
```

This introduces a `Foo` via `foo`, then applies `force` to each of its fields
and display the results. This prints the following:
```hs
True
'T'
```



Depending on the algorithm, the computation associated with `x` might be
expensive, and in a regular program `x` would be a good candidate for
`let`-binding, or some other strategy that effects sharing of its results
without recomputing them. 

Recall that `Optimal` has a lazy, evaluate-at-most-once semantics. That means
that the first occurrence of `force (x f)` will evaluate the computation
associated with `x`

In `Optimal`, every binding acts as a shared computation, local to a particular
instance to a module. What this means 

`force`ing `x` is very fast; to evaluate a hard-coded number is practically
immediate. `force`ing `y` takes much longer, however, because the computation is
expensive. `y`, then, is a good candidate for reuse. 



TODO: this example is too contrived. Need an example that really demonstrates
automatic reuse of `y`, not just something that can more easily be accomplished
by `let`-binding it

---

++++ Nothing polished below this point ++++


```diff
-   force (x f) >>= print
    force (y f) >>= print
+   force (x f) >>= print
```





The typical programming solution to this problem is to bind an intermediate
value and reuse that value across the various branches of computation that
require it. `Optimal` seeks to automate this: 







- Within a Haskell project, can use `optimal` quasiquoter to write "module" of
  names binding expressions
  - Syntax tutorial
- Can introduce "regular" values or vectors of values
  - `L` need not have a notion of lists!
- Can eliminate lists of values
- Outside the quasiquoter, modules generate functions, which you can use in
  IO(-like) environments


## How does Optimal(L) work?

- Analyzes expressions to determine need for for expression reuse
  - Free variable analysis
- Allocates mutable references per-bound-expression
- Evaluating an expression both evaluates it and mutates the reference to
  contain the result
- All references to a (named) expression refer to the same mutable memory, so
  they will either evaluate the expression or use the cached result


## How do I make a new L?

If you can create a translation of your language to template haskell expressions
(`Exp`s), `Optimal` can do the rest.

- Create a semantics of your language interpretable in Haskell
  - At the moment, I think this amounts to a TH translation

- I am trying to push later the point at which `L` is translated to Haskell
  - Instead, I'm trying to describe the semantics it needs to have in order to
    make its way through the compilation process
  
- Is there a way to describe the semantics fully enough that we can automate a
  translation to TH?
  - Almost certainly not

- If translation is required, why go to the trouble of doing all of this in
  advance of translation?


