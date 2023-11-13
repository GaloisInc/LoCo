# DRAFT

# Introduction

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


# Language Constructs

`Optimal(L)` is embedded in Haskell. This is true regardless of the choice of
`L` (`L`, recall, is also limited to being Haskell at the moment). Users of
`Optimal` import the library and write their `Optimal` code in a quasiquoted
block, using the `optimal` quoter.


## Types

Within this quoter, users can declare types. The core type construct in
`Optimal` is the "module":
```hs
[optimal|
type Foo = { x : Bool, y : Char }
|]
```

This says that a `Foo` is a "module" with fields `x` of type `Bool` and `y` of
type `Char`. A module is like a struct, or a record: it is a collection of
values accessible by identifiers. `Optimal` modules are more permissive than
these constructs, though: what `Foo` *actually* promises is that any implementor
exposes *at least* fields `x` and `y` of the specified types - implementors are
free to bind arbitrarily more fields than that, which can aid in the computation
of the exposed fields.

Note that we have no definition in `Optimal` of `Bool` or `Char`. When
encountering "unbound" types like this, `Optimal` will try to generate code that
relies on a Haskell type with that name. Subsequent declarations referring to
`Foo`, though, will use our declared type.

Users can also declare type aliases:
```hs
[optimal|
type Bar = Foo
|]
```

`Optimal` also supports vector types, denoted as `[<typename>]`:
```hs
[optimal|
type Bitvec = [Bool]
|]
```

See below for more on vector semantics.


## Modules

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
refer to values defined outside the scope of an `Optimal` quotation). See below
for more detail on why these expressions use `pure`.

We've written the module by declaring `x` before declaring `y`, but this is not
necessary, even though `y` mentions `x`. A side effect of `Optimal`'s efforts to
share computation mean that declarations in generated Haskell code are
automatically topologically reordered.

TODO: well-typedness?

TODO: commutative monads?

This generates a few functions:
```hs
foo :: MonadIO m => m (Foo m)
x :: MonadIO m => Foo m -> Thunked m Bool
y :: MonadIO m => Foo m -> Thunked m Char
```

The Haskell type generated for `Foo` can be considered a variation on a built-in
Haskell record type, with `foo` as its constructor and `x` and `y` as its
eliminators. (Technically, `x` and `y` are not generated as standalone
functions, but rather as record accessors.) The extra monadic machinery and
appearance of the `Thunked` type are indications of the special semantics that
`Optimal` imposes on this declaration.

Users need to know little about `Thunked` values, other than the type of their
eliminator:
```hs
force :: MonadIO m => Thunked m a -> m a
```

A `Thunked` value can be thought of as a suspended computation, and `force`
evaluates that computation to yield its result. TODO it does more.


## Client Code

With this in mind, we can write some client code for this module, in `IO` for
simplicity's sake:
```hs
workWithFoo :: IO ()
workWithFoo =
  do
    f <- foo

    let xThunk :: Thunked m Bool
        xThunk = x f
        yThunk :: Thunked m Char
        yThunk = y f
        
    force xThunk >>= print
    force yThunk >>= print
```

This creates a `Foo` via `foo`, then applies `force` to each of its fields and
display the results. This prints the following:
```hs
True
'T'
```


# Evaluation

So far, if you squint, using `Optimal` looks kinda like just using a language,
with a bit more syntactic noise and evaluation overhead.

Recall that `Optimal` is explicitly lazy. This means that the creation of `f`
does not trigger evaluation of the expressions bound within it. Furthermore,
accessing the fields `x` and `y` do nothing more than expose the
computations-in-waiting - they do not evaluate them. Forcing `xThunk` is the
first time the computation associated with `x` is performed - likewise for
forcing `yThunk`.

Recall also that `Optimal` evaluates things at most once. This means that once
`xThunk` is forced, its result is cached indefinitely, and any other
computations in the module that refer to `x` by name can leverage this caching.
So, when `yThunk` is forced, it does not recalculate the result of `x` - it
instead refers to the cached result, and is able to evaluate to `'T'`
immediately.

We happened to order the `force` invocations in dependency order - `y` depends
on `x`, as declared, and we forced `x` first and `y` second. However, suppose we
reorder the `force`s and rerun the code:
```diff
-   force xThunk >>= print
    force yThunk >>= print
+   force xThunk >>= print
```

There's obviously no way to avoid computing `x` in order to display the result
of `y`, so you can expect `x`'s computation to be evaluated by the time `y`'s
result is printed. However, `Optimal`'s caching/sharing semantics means that
forcing `xThunk` will leverage the result that was computed when forcing
`yThunk`, so `x`'s result prints immediately after `y`'s!


# Source Language Details

At the moment, as mentioned, the only choice of source language (`L`) is
Haskell. In principle, the quickest way to support a new language is to create a
compilation procedure to convert the language into Template Haskell expressions
(`Exp`s).

Since `Optimal` is embedded in Haskell regardless of the source language,
though, the language and its `Exp` representation need to meet the typing
requirements of `Optimal`.


## Expressions

Expression-binding looks like this:
```
...
  x = <| pure (isPrime 30000001) |>,
...
```

When a module binds an expression like this, and when the identifier is exposed
in the module's interface as type `T`, the expression needs to have the Haskell
type `MonadIO m => m T`. `x` was declared in its module's type as `Bool`, so
this expression types as `MonadIO m => m Bool`. Recall that `pure` is used
repeatedly above - it is the trivial monadic embedding for computations that
don't require monadic facilities.


## Vectors

TODO


## Tuple Patterns

TODO

- Can introduce "regular" values or vectors of values
  - `L` need not have a notion of lists!
- Can eliminate lists of values



---

++++ Nothing polished below this point ++++


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


# Reference

## Types

`Optimal` declarations can make use of any in-scope Haskell type, but users can
also declare their own types:
```
TYDECL ::= 'type' TYIDENT '=' TYPE

TYIDENT ::= [A-Z][a-z0-9_]*

TYPE
  ::= TYIDENT
    | '{' (VARIDENT ':' TYPE ',')+ '}'
    | [TYIDENT]
```

