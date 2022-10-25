# LoCo Syntax by Example

Consider the format defined by a contiguous collection (i.e. an 8-byte chunk) of
two unsigned (wlog big-endian) 32-bit integers, called `x` and `y`, appearing in
that order. We want to describe this format in LoCo. 

We will begin with a subset of the language that should, modulo keywords, be
recognizable to someone with experience in DDLs.

```
defineMEP TwoInts =
  { x := UInt32
  , y := UInt32
  }
```

Think of `x` and `y` as *values* having type `u32`. Think of a *value* at a
particular type as a regular, concrete inhabitant of that type.

`MEP` in `defineMEP` stands for `M`ulti-`E`ntrypoint `P`arser. One of LoCo's
motivations is to bootstrap creation of efficient multi-entry-point parsers for
complex formats - that is, using the same data format declaration(s) to
instantiate potentially several parsers, each of which targets a subset of the
format being described.

Extraction of single-entrypoint parsers from MEPs occurs solely at the
application level - that is, LoCo does not have in-language constructs for
declaring non-multi-entrypoint parsers. Users can still extract fields of MEPs
for use in other MEPs, though, using dot-notation syntax:

```
defineMEP TwoOtherInts =
  { x := TwoInts.x
  , y := UInt32
  }
```

<!-- At this point, you can think of the types of `x` and `y` as unsigned
32-bit integer. -->

The philosophy `TwoOtherInts` should then be able to parse `TwoInts`'s member
`x` without parsing its `y`. This specialization obviously doesn't provide
substantial efficiency gains with this format, but as we introduce more complex
formats, that calculus will change.

# Locations

One problem with our declaration syntax as it stands is that it's implicitly
sequential - i.e., that `x` is followed by `y`. In LoCo, we remove the
assumption of sequentiality, making explicit locations at which data are
sourced.

One way of accomplishing this is to fully concretize the variables' locations,
applying the parsers *at* particular ranges of locations:

```
defineMEP TwoInts =
  { x := UInt32 @[0, 4)@
  , y := UInt32 @[4, 8)@
  }
```

The syntactic form we introduce here is `@[<start-offset>, <end-offset>)`, which
describes a right-open range of integer byte offsets *at* which the value is
located. In this case, the offsets are concrete integer values.

This usage is not incorrect, and indeed suffices to describe this format, but
only so long as it is the prefix of a document, since we chose the concrete
location `0` to anchor this document. To be able to locate this format elsewhere
in a document, however, we would like to be able to abstract over its location.
In LoCo, that looks like this:

```
defineMEP TwoInts @[L0, L1, L2)@ =
  { x := UInt32 @[L0, L1)@
  , y := UInt32 @[L1, L2)@
  }
```

This change is substantial, so we'll take it little by little.

We're replacing concrete offsets with *symbolic locations* named `L0`, `L1`, and
`L2`. These values can be (duck-)typed as integer byte offsets, but can
potentially be populated with any value, subject to some constraints.

One such constraint is evident in the MEP's signature with the extended range
syntax `@[L0, L1, L2)`. This syntax[^1] specifies a *range parameter* to the
MEP. Range parameters are comprised of several *location parameters*. A range
parameter's format is a generalization of classic range notation: our syntax
allows for two or more locations grouped together such that a location named
further left in the range comes before one that's named further right. In this
case, that means that `L0` <= `L1` <= `L2`, with `L0` as the (inclusive)
beginning of the range and `L2` as the (exclusive) end.

We're then using these named locations to specify the locations of `x` and `y`
*relative to one another* in this format. Since `x` is parsed at `[L0, L1)`, it
is the first integer in the chunk under parse (since `L0` comes first based on
our range-notation rules). Since `y` is parsed at `[L1, L2)`, we know that it's
located immediately after `x`, with no intervening bytes, because `x` ends at
`L1`.

That we have provided exactly one range parameter means that we can expect this
format to inhabit one contiguous range of bytes - and that behavior is borne out
in the format's definition.

Replacing concrete offsets with symbolic ones, then, maintains almost all of the
information originally conveyed - what's missing is an explicit range "width"
that we got for free, arithmetically, with concrete offsets. That is to say, it
is obvious that `[4, 8)` is a 4-wide range, while the same can't be said of
`[L1, L2)`.

TODO: width notation

Our previously-introduced dot-notation operator extends relatively naturally to
this expanded syntax. MEP members derived from other MEPs still require location
arguments - the form and arity of which are defined by the (primitive or
user-defined) parser's signature.

```
defineMEP TwoOtherInts @[L0, L1, L2)@ =
  { x := TwoInts.x @[L0, L1)@
  , y := UInt32 @[L1, L2)@
  }
```

`TwoInts.x` is a `UInt32`, which we know expects a single range parameter
comprised of two location parameters, so we abstract over the necessary
individual locations in `TwoOtherInts`'s signature to provide that.

As mentioned, this procedure expands naturally to dot-accesses resolving to
user-defined parsers:

```
defineMEP ThreeInts @[L0, L1, L2, L3)@ =
  { xy := TwoOtherInts @[L0, L1, L2)@
  , z := UInt32 @[L2, L3)@
  }
```

<!-- MARK: this is funky - `TwoIntsX` *should* only parse a subset of the range
that `TwoInts` otherwise needs, but if we don't introduce the full range in
its signature we can't pass the number of location parameters that `TwoInts`
expects:

```
defineSEP TwoIntsX @[L0, L1, ?) = extract (TwoInts @[L0, L1, L2)) x
defineSEP TwoIntsX @[L0, L1, ?) = (TwoInts @[L0, L1, L2)) `extract` x
defineSEP TwoIntsX @[L0, L1, ?) = extract x from (TwoInts @[L0, L1, L2))
``` -->

# Seek

Some formats are described non-sequentially, requiring "seeking" around the
document to parse them. Let's adjust our format's semantics to consider this.
Instead of parsing two integers immediately adjacent to one another, we want to
look for two integers separated from one another. In LoCo, we would express this
like so:

```
defineMEP TwoInts @[L0, L1) [L2, L3)@ =
  { x := UInt32 @[L0, L1)@
  , y := UInt32 @[L2, L3)@
  }
```

Note that the MEP is no longer defined with a single range parameter, but rather
with two. This arity change denotes potential *seeking* behavior: we no longer
know that this format occurs in precisely one contiguous range of bytes, rather,
we know that it occurs across precisely two.

Note also that, in the MEP's body, we've correspondingly changed `y`'s range to
remove the syntactic guarantee that `y` starts where `x` ends.

## Ad-hoc Locations

Many non-sequential formats encode information on where to seek in the values
that inhabit them, requiring parsing of some values to know where to parse
others.

To explore this, let's rename `x` in our format to `offset`:

```
defineMEP TwoInts @[L0, L1) [L2, L3)@ =
  { offset := UInt32 @[L0, L1)@
  , y := UInt32 @[L2, L3)@
  }
```

Then, let's adjust our format's semantics again. We want to parse one integer
representing an offset, and one integer *located at that offset*. In LoCo, we
can express this semantics like so:

```
defineMEP TwoInts @[L0, L1) [*, *)@ =
  { offset := UInt32 @[L0, L1)@
  , y := UInt32 @[TOLOC(offset), TOLOC(offset) + 4)@
  }
```

Again, several things changed.

The locations in the second range parameter can no longer be provided by the
caller/client, as was the case before (TODO make callee/caller responsibility
explicit earlier?). However, the caller must still know that `TwoInts` occurs
across more than one contiguous range. To inhabit this middle ground, we
introduce asterisks as a form of "known unknown".

`offset` exists in this format as a `u32` *value*, but we need to interpret it
as a *location*, a type-level difference. For that, we introduce `TOLOC`, an
operator that converts *value*s into *location*s. 

(Assume for now that the only sort of location is an arbitrary-width integer,
and that any fixed-width integer can be upcasted trivially and implicitly.)

As established earlier, locations are duck-typed as integer byte offsets. One
impact of this is that they support simple arithmetic operations. To specify
`y`'s range's end, we simply add 4 (the byte width of a 32-bit integer) to
`TOLOC(offset)`.


# Callers

Readers may have developed a mental model of the caller-callee parser
relationship by now - more concrete details of its semantics follow. We've thus
far left `UInt32` undefined. In truth, it is a primitive, but we can offer its
prototype to demonstrate what calling entails:

```
defineSEP UInt32 @[L0, L1)@ = <primitive>
```

It is declared with a single range parameter consisting of two location
parameters, which directly matches how it's called throughout the above
examples. It's a `S`ingle-`E`ntrypoint `P`arser, hence `defineSEP` - this
distinction has no impact on calling convention, however, but rather makes clear
that the value it parses is in a sense atomic.

TODO: encode information on its width?


# Constraints

In/out of band

# `Many`

With seek?


[^1]:
  ```
  RANGE ::= '@' '[' LOC ',' LOCS ')'
  LOC ::= <variable-name>
  LOCS ::= LOC
         | LOC ',' LOCS
  ```
