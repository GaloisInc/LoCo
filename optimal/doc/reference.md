- [Language Constructs](#language-constructs)
  - [Types](#types)
    - [Modules](#modules)
    - [Aliases](#aliases)
    - [Vectors](#vectors)
  - [Expressions](#expressions)
    - [Code Generation](#code-generation)
  - [Vectors](#vectors-1)
    - [`generate`](#generate)
      - [Code Generation](#code-generation-1)
    - [`replicate`](#replicate)
    - [`map`](#map)
    - [`index`](#index)
    - [TODO: Modules with Parameters](#todo-modules-with-parameters)
- [User Code](#user-code)
  - [`Thunked`](#thunked)
  - [Expressions](#expressions-1)
  - [`Vector`](#vector)
  - [Vectors](#vectors-2)
- [Evaluation Semantics](#evaluation-semantics)
  - [Expressions](#expressions-2)
  - [Vectors](#vectors-3)
- [Source Language Details](#source-language-details)
  - [Typing Rules](#typing-rules)
    - [Expressions](#expressions-3)
    - [Vectors](#vectors-4)
      - [`generate`](#generate-1)
      - [`replicate`](#replicate-1)
      - [`map`](#map-1)
      - [`index`](#index-1)
    - [Tuple Patterns](#tuple-patterns)

# Language Constructs

`Optimal(L)` is embedded in Haskell. This is true regardless of the choice of
`L` (`L`, recall, is also limited to being Haskell at the moment). Users of
`Optimal` import the library and write their `Optimal` code in a quasiquoted
block, using the `optimal` quoter.


## Types

Within this quoter, users can declare types.

### Modules

The core type construct in `Optimal` is the "module":
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

### Aliases

Users can also declare type aliases:
```hs
[optimal|
type Baz = Foo
|]
```


### Vectors

Users can also declare vector types:
```hs
[optimal|
type Boo = Vec<Bool>
|]
```

See [below](#vectors-1) for more details on vectors.


## Expressions

We'll start with a module that just uses plain expressions:

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
refer to values defined outside the scope of an `Optimal` quotation).

We've written the module by declaring `x` before declaring `y`, but this is not
necessary, even though `y` mentions `x`. A side effect of `Optimal`'s efforts to
share computation mean that declarations in generated Haskell code are
automatically topologically reordered.

See [below](#expressions-3) for details on the typing rules that govern these
expressions.

TODO: commutative monads?

### Code Generation

This generates a few functions:
```hs
foo :: MonadIO m => m (Foo m)
x :: Foo m -> Thunked m Bool -- caveat: record accessor
y :: Foo m -> Thunked m Char -- caveat: record accessor
```

The Haskell type generated for `Foo` can be considered a variation on a built-in
Haskell record type, with `foo` as its constructor and `x` and `y` as its
eliminators. The extra monadic machinery and appearance of the `Thunked` type
are indications of the special semantics that `Optimal` imposes on this
declaration.

See below for details on how to interact with `Thunked` values.

## Vectors

Modules can also contain vector-type fields:

```hs
[optimal|
type Bar = { xs : Vec<Bool> }
|]
```

This says that a `Bar` is a module with fields `xs` of type `Vec<Bool>`, or
vector of `Bool`.

### `generate`

One vector introduction form is `generate`:

```hs
[optimal|
bar : Bar
bar = {
  xs = generate 100 <| \idx -> pure (isPrime idx) |>,
}
|]
```

It defines a module `bar` of type `Bar`. It populates the field named `xs` with
a 100-element vector of `Bool`s. Each `Bool` represents whether or not its index
(zero-based) in the vector is prime. So, the zeroth, first, and second elements
will be `False`, the third will be `True`, and so on.

The length of a vector can also be specified by identifier, if the length needs
to be calculated based on prior computation:
```hs
[optimal|
bar' : Bar
bar' = {
  xsLen = <| pure 100 |>,
  xs = generate xsLen <| \idx -> pure (isPrime idx) |>,
}
|]
```

See [below](#vectors-4) for more details on the typing rules governing vectors,
different vector syntactic forms, and more detailed semantics.


#### Code Generation

This generates a few functions:
```hs
bar :: MonadIO m => m (Bar m)
bar' :: MonadIO m => m (Bar m)
xs :: Bar m -> Thunked m (Vector m Bool) -- caveat: record accessor
```

This introduces a new type, `Vector`. `Optimal` exposes this type, as well as a
simple API for it, which is used in code generation and is available to end
users.

See [below](#vector) for details on how to interact with `Vector`s.


### `replicate`

Vectors can also be created via `replicate`:
```hs
[optimal|
bar : Bar
bar = {
  xs = replicate 100 <| pure True |>,
}
|]
```

Unlike `generate`, the fill expression is not a function, but rather a monadic
expression. Like `generate`, the length can also be provided as an identifier.


### `map`

Vectors can be transformed via `map`:
```hs
[optimal|
bar : Bar
bar = {
  xs = generate 100 <| \idx -> pure (isPrime idx) |>,
  ys = map xs <| \element -> pure (not element) |>,
}
|]
```


### `index`

One elimination form of vectors is `index`:

```hs
[optimal|
bar : Bar
bar = {
  xs = generate 100 <| \idx -> pure (isPrime idx) |>,
  x = index xs 0,
}
|]
```

This binds `x` to the zeroth element of the vector `xs`. `x` can be treated as a
`Bool` in future `Optimal` computations. Note that `Bar`'s type does not mention
an `x` field. If it did, it would be a `Bool`, and an `x` accessor would exist
with the following type:
```hs
x :: Bar m -> Thunked m Bool -- caveat: record accessor
```

The index parameter can also be provided to `index` as an identifier.


### TODO: Modules with Parameters


# User Code

Many of the below snippets can be found in `Language.Optimal.Samples.Doc`.

## `Thunked`

Users need to know little about `Thunked` values in order to work with them,
other than the type of their eliminator:
```hs
force :: MonadIO m => Thunked m a -> m a
```

A `Thunked` value can be thought of as a suspended computation, and `force`
evaluates that computation to yield its result. See
[below](#evaluation-semantics) for more detail on this process.


## Expressions

With `force` in mind, we can write some client code for `foo`, in `IO` for
simplicity's sake:
```hs
workWithFoo :: IO ()
workWithFoo =
  do
    f <- foo

    let xThunk :: Thunked IO Bool
        xThunk = x f
        yThunk :: Thunked IO Char
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


## `Vector`

`Vector`s appear in modules as `Thunked` values, so they need to be `force`d
before they can be manipulated. 

The main eliminator for `Vector` is `vIndex :: Vector m a -> Int -> m a`. It
indexes a vector and `force`s the element at that index.

## Vectors

With this in mind, we can write some client code for `bar`, in `IO` for
simplicity's sake:
```hs
workWithBar :: IO ()
workWithBar =
  do
    b <- bar

    let xsThunk :: Thunked IO (Vector IO Bool)
        xsThunk = xs b

    xsVec <- force xsThunk

    vIndex xsVec 0 >>= print
```

This creates a `Bar` via `bar`, then `force`s `xs` and indexes it. This prints
the following:
```hs
True
```


# Evaluation Semantics

So far, if you squint, using `Optimal` looks a little like a regular language,
with a bit more syntactic noise and evaluation overhead.

## Expressions

Recall that `Optimal` is explicitly lazy. This means that the creation of `f`
[above](#expressions-1), does not trigger evaluation of the expressions bound
within it. Furthermore, accessing the fields `x` and `y` do nothing more than
expose the computations-in-waiting - they do not evaluate them. Forcing `xThunk`
is the first time the computation associated with `x` is performed - likewise
for forcing `yThunk`.

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


## Vectors

Vectors are lazy in `Optimal`. Operations on one element do not affect
operations on other elements. One such operation is `force`: forcing the
computation of one element of a vector will not force the computation of any
others. Also, as with expressions, forcing the same element a second time will
reuse the previously-computed value.

This laziness persists through transformations. Forcing one element of a vector
produced by applying `map` to another will force the new element and the
original element, but not necessarily other elements of either vector.


# Source Language Details

At the moment, as mentioned, the only choice of source language (`L`) is
Haskell. In principle, the quickest way to support a new language is to create a
compilation procedure to convert the language into Template Haskell expressions
(`Exp`s).

Since `Optimal` is embedded in Haskell regardless of the source language,
though, the language and its `Exp` representation need to meet the typing
requirements of `Optimal`.

## Typing Rules

### Expressions

Expression-binding looks like this:
```hs
[optimal|
...
  x = <| pure (isPrime 30000001) |>,
...
|]
```

When a module binds an expression like this, and when the identifier is exposed
in the module's interface as type `T`, the expression needs to have the Haskell
type `MonadIO m => m T`. `x` was declared in its module's type as `Bool`, so
this expression types as `MonadIO m => m Bool`. Recall that `pure` is used
repeatedly above - it is the trivial monadic embedding for computations that
don't require monadic facilities.


### Vectors

Generally, a vector with `Optimal` type `Vec<T>` has Haskell type `Vector m T`.
It may appear as `Thunked m (Vector m T)`, depending on the context.

#### `generate`
```hs
[optimal|
...
  vectorLit = generate 5 <| \i -> pure (i + 1) |>,
  
  vectorLength = <| pure 5 |>,
  vectorSyn = generate vectorLength <| \i -> pure (i + 1) |>,
...
|]
```

`generate` also requires a length parameter and a fill parameter. The length
parameter can be an identifier or an integer literal, and the fill parameter is
an expression. The length parameter, if an identifier, needs to refer to an
expression of type `(Integral a, MonadIO m) => m a`. typing rules are as above,
but the fill parameter must have the Haskell type `MonadIO m => Int -> m T`, for
a vector with Optimal type `Vec<T>`. Intuitively, the fill expression is a
function, and Optimal passes the index being generated as a parameter to the
function.


#### `replicate`
```hs
[optimal|
...
  vectorLit = replicate 5 <| pure 'A' |>,

  vectorLength = <| pure 5 |>,
  vectorSym = replicate vectorLength <| pure 'A' |>,
...
|]
```

Vector introduction via `replicate` requires two parameters: a length parameter
and a fill parameter. Both have typing rules as above, but the fill parameter
must have the Haskell type `MonadIO m => m T`, for a vector with Optimal type
`Vec<T>`.


#### `map`

```hs
[optimal|
...
  vector = generate 5 <| \i -> pure (i + 1) |>,
  newVector = map vector <| \element -> pure (even element) |>,
...
|]
```

`map` takes a vector parameter and a transformer parameter. The vector parameter
is an identifier, and the transformer is an expression. If the original vector
has Optimal type `Vec<T>`, then the transforming expression should have Haskell
type `MonadIO m => T -> m U` to produce a new vector of Optimal type `Vec<U>`.


#### `index`

Vectors can also be indexed, via an `index` construct:
```hs
[optimal|
...
  vector = generate 5 <| \i -> pure (i + 1) |>,
  vectorIndex = <| pure 2 |>,
  vectorElem = index vector vectorIndex,
...
|]
```

`index` takes a vector parameter and an index parameter. The vector parameter is
an identifier, and the index parameter can be either an identifier or an integer
literal. If the result has Optimal type `T`, the vector must have Optimal type
`Vec<T>` and the index must have Haskell type `Int`.

TODO
- a bit too much bouncing between Haskell and Optimal types?


### Tuple Patterns

TODO



<!--

- Can introduce "regular" values or vectors of values
  - `L` need not have a notion of lists!
- Can eliminate lists of values

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
    | 'Vec' '<' TYIDENT '>'
```
-->
