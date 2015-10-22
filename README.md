# Batteries

[![Package version](https://img.shields.io/bower/v/purescript-batteries.svg)](https://github.com/tfausak/purescript-batteries/releases)

A PureScript prelude with more features.

PureScript is a powerful language with a granular package ecosystem. It can
take tens of lines of imports do accomplish trivial tasks. Batteries makes
things better by giving you more functionality from a single import.

To get started, add Batteries as a dependency to your `bower.json`:

``` json
{ "dependencies": { "purescript-batteries": "~0.1.0" } }
```

Once you install it, use it by importing it unqualified:

``` purescript
import Batteries
```

Batteries includes a few modules that clash with each other. To use them,
import them qualified:

``` purescript
import qualified Batteries.Array as Array
```

That's it! If you know of a library that should be included, please open a pull
request. And if Batteries doesn't export something that you need from one of
the included libraries, please open an issue.

Batteries includes the following libraries:

- [purescript-aff](https://github.com/slamdata/purescript-aff)
- [purescript-arrays](https://github.com/purescript/purescript-arrays)
- [purescript-assert](https://github.com/purescript/purescript-assert)
- [purescript-bifunctors](https://github.com/purescript/purescript-bifunctors)
- [purescript-catenable-lists](https://github.com/purescript/purescript-catenable-lists)
- [purescript-console](https://github.com/purescript/purescript-console)
- [purescript-const](https://github.com/purescript/purescript-const)
- [purescript-contravariant](https://github.com/purescript/purescript-contravariant)
- [purescript-control](https://github.com/purescript/purescript-control)
- [purescript-coproducts](https://github.com/purescript/purescript-coproducts)
- [purescript-datetime](https://github.com/purescript/purescript-datetime)
- [purescript-distributive](https://github.com/purescript/purescript-distributive)
- [purescript-eff](https://github.com/purescript/purescript-eff)
- [purescript-either](https://github.com/purescript/purescript-either)
- [purescript-enums](https://github.com/purescript/purescript-enums)
- [purescript-exceptions](https://github.com/purescript/purescript-exceptions)
- [purescript-exists](https://github.com/purescript/purescript-exists)
- [purescript-foldable-traversable](https://github.com/purescript/purescript-foldable-traversable)
- [purescript-foreign](https://github.com/purescript/purescript-foreign)
- [purescript-free](https://github.com/purescript/purescript-free)
- [purescript-functions](https://github.com/purescript/purescript-functions)
- [purescript-generics](https://github.com/purescript/purescript-generics)
- [purescript-globals](https://github.com/purescript/purescript-globals)
- [purescript-identity](https://github.com/purescript/purescript-identity)
- [purescript-inject](https://github.com/purescript/purescript-inject)
- [purescript-integers](https://github.com/purescript/purescript-integers)
- [purescript-invariant](https://github.com/purescript/purescript-invariant)
- [purescript-lazy](https://github.com/purescript/purescript-lazy)
- [purescript-lists](https://github.com/purescript/purescript-lists)
- [purescript-maps](https://github.com/purescript/purescript-maps)
- [purescript-math](https://github.com/purescript/purescript-math)
- [purescript-maybe](https://github.com/purescript/purescript-maybe)
- [purescript-monoid](https://github.com/purescript/purescript-monoid)
- [purescript-nullable](https://github.com/paf31/purescript-nullable)
- [purescript-parallel](https://github.com/purescript/purescript-parallel)
- [purescript-prelude](https://github.com/purescript/purescript-prelude)
- [purescript-proxy](https://github.com/purescript/purescript-proxy)
- [purescript-random](https://github.com/purescript/purescript-random)
- [purescript-refs](https://github.com/purescript/purescript-refs)
- [purescript-sets](https://github.com/purescript/purescript-sets)
- [purescript-st](https://github.com/purescript/purescript-st)
- [purescript-strings](https://github.com/purescript/purescript-strings)
- [purescript-tailrec](https://github.com/purescript/purescript-tailrec)
- [purescript-these](https://github.com/purescript/purescript-these)
- [purescript-transformers](https://github.com/purescript/purescript-transformers)
- [purescript-tuples](https://github.com/purescript/purescript-tuples)
- [purescript-unfoldable](https://github.com/purescript/purescript-unfoldable)
- [purescript-unsafe-coerce](https://github.com/purescript-contrib/purescript-unsafe-coerce)
