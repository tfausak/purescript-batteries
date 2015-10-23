# Batteries

[![Build status](https://img.shields.io/travis/tfausak/purescript-batteries.svg)](https://travis-ci.org/tfausak/purescript-batteries)
[![Package version](https://img.shields.io/bower/v/purescript-batteries.svg)](https://github.com/tfausak/purescript-batteries/releases)

:battery: A PureScript prelude with more features.

PureScript is a powerful language. Unfortunately the package ecosystem is
excessively granular. That means it can take tens of lines of imports to
accomplish trivial tasks. Batteries makes things better by giving you more
functionality from a single import.

To install Batteries, add it as a dependency to your `bower.json`:

``` js
{
  "dependencies": {
    "purescript-batteries": "~0.1.0",
    // Other dependencies go here.
  },
  // Other package stuff goes here.
}
```

Then run `pulp dep install`. Once that's done, use Batteries like any other
module by importing it. It is designed to be imported unqualified:

``` purescript
import Batteries
```

That's all there is to it!

If you want to see exactly what Batteries exports, check out [the source][]. If
it's missing something or including something it shouldn't, please open [an
issue][]!

The other way to use Batteries is to *not* import it. Instead, simply add it as
a dependency and use the packages that it includes. In this way it is similar
to [purescript-base][].

Batteries includes the following packages:

- [aff](https://github.com/slamdata/purescript-aff)
- [arrays](https://github.com/purescript/purescript-arrays)
- [arrows](https://github.com/purescript/purescript-arrows)
- [assert](https://github.com/purescript/purescript-assert)
- [bifunctors](https://github.com/purescript/purescript-bifunctors)
- [catenable-lists](https://github.com/purescript/purescript-catenable-lists)
- [console](https://github.com/purescript/purescript-console)
- [const](https://github.com/purescript/purescript-const)
- [contravariant](https://github.com/purescript/purescript-contravariant)
- [control](https://github.com/purescript/purescript-control)
- [coproducts](https://github.com/purescript/purescript-coproducts)
- [datetime](https://github.com/purescript/purescript-datetime)
- [distributive](https://github.com/purescript/purescript-distributive)
- [eff](https://github.com/purescript/purescript-eff)
- [either](https://github.com/purescript/purescript-either)
- [enums](https://github.com/purescript/purescript-enums)
- [exceptions](https://github.com/purescript/purescript-exceptions)
- [exists](https://github.com/purescript/purescript-exists)
- [foldable-traversable](https://github.com/purescript/purescript-foldable-traversable)
- [foreign](https://github.com/purescript/purescript-foreign)
- [free](https://github.com/purescript/purescript-free)
- [functions](https://github.com/purescript/purescript-functions)
- [generics](https://github.com/purescript/purescript-generics)
- [globals](https://github.com/purescript/purescript-globals)
- [identity](https://github.com/purescript/purescript-identity)
- [inject](https://github.com/purescript/purescript-inject)
- [integers](https://github.com/purescript/purescript-integers)
- [invariant](https://github.com/purescript/purescript-invariant)
- [lazy](https://github.com/purescript/purescript-lazy)
- [lists](https://github.com/purescript/purescript-lists)
- [maps](https://github.com/purescript/purescript-maps)
- [math](https://github.com/purescript/purescript-math)
- [maybe](https://github.com/purescript/purescript-maybe)
- [monoid](https://github.com/purescript/purescript-monoid)
- [nullable](https://github.com/paf31/purescript-nullable)
- [parallel](https://github.com/purescript/purescript-parallel)
- [prelude](https://github.com/purescript/purescript-prelude)
- [profunctor](https://github.com/purescript/purescript-profunctor)
- [proxy](https://github.com/purescript/purescript-proxy)
- [random](https://github.com/purescript/purescript-random)
- [refs](https://github.com/purescript/purescript-refs)
- [semirings](https://github.com/purescript/purescript-semirings)
- [sequences](https://github.com/hdgarrood/purescript-sequences)
- [sets](https://github.com/purescript/purescript-sets)
- [st](https://github.com/purescript/purescript-st)
- [strings](https://github.com/purescript/purescript-strings)
- [tailrec](https://github.com/purescript/purescript-tailrec)
- [these](https://github.com/purescript/purescript-these)
- [transformers](https://github.com/purescript/purescript-transformers)
- [tuples](https://github.com/purescript/purescript-tuples)
- [unfoldable](https://github.com/purescript/purescript-unfoldable)
- [unsafe-coerce](https://github.com/purescript_TODO_contrib/purescript-unsafe-coerce)
- [validation](https://github.com/purescript/purescript-validation)

To see the exact versions that Batteries depends on, look at the
[`bower.json`][].

[the source]: ./src/Batteries.purs
[an issue]: https://github.com/tfausak/purescript-batteries/issues
[purescript-base]: https://github.com/purescript-contrib/purescript-base
[`bower.json`]: ./bower.json
