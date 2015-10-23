# Batteries

[![Build status](https://img.shields.io/travis/tfausak/purescript-batteries.svg)](https://travis-ci.org/tfausak/purescript-batteries)
[![Package version](https://img.shields.io/bower/v/purescript-batteries.svg)](https://github.com/tfausak/purescript-batteries/releases)

A PureScript prelude with more features.

PureScript is a powerful language with a granular package ecosystem. It can
take tens of lines of imports to accomplish trivial tasks. Batteries makes
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

That's it! If you know of a package that should be included, please open a pull
request. And if Batteries doesn't export something that you need from one of
the included packages, please open an issue.

Batteries includes the following packages:

- <https://github.com/slamdata/purescript-aff>
- <https://github.com/purescript/purescript-arrays>
- <https://github.com/purescript/purescript-arrows>
- <https://github.com/purescript/purescript-assert>
- <https://github.com/purescript/purescript-bifunctors>
- <https://github.com/purescript/purescript-catenable-lists>
- <https://github.com/purescript/purescript-console>
- <https://github.com/purescript/purescript-const>
- <https://github.com/purescript/purescript-contravariant>
- <https://github.com/purescript/purescript-control>
- <https://github.com/purescript/purescript-coproducts>
- <https://github.com/purescript/purescript-datetime>
- <https://github.com/purescript/purescript-distributive>
- <https://github.com/purescript/purescript-eff>
- <https://github.com/purescript/purescript-either>
- <https://github.com/purescript/purescript-enums>
- <https://github.com/purescript/purescript-exceptions>
- <https://github.com/purescript/purescript-exists>
- <https://github.com/purescript/purescript-foldable-traversable>
- <https://github.com/purescript/purescript-foreign>
- <https://github.com/purescript/purescript-free>
- <https://github.com/purescript/purescript-functions>
- <https://github.com/purescript/purescript-generics>
- <https://github.com/purescript/purescript-globals>
- <https://github.com/purescript/purescript-identity>
- <https://github.com/purescript/purescript-inject>
- <https://github.com/purescript/purescript-integers>
- <https://github.com/purescript/purescript-invariant>
- <https://github.com/purescript/purescript-lazy>
- <https://github.com/purescript/purescript-lists>
- <https://github.com/purescript/purescript-maps>
- <https://github.com/purescript/purescript-math>
- <https://github.com/purescript/purescript-maybe>
- <https://github.com/purescript/purescript-monoid>
- <https://github.com/paf31/purescript-nullable>
- <https://github.com/purescript/purescript-parallel>
- <https://github.com/purescript/purescript-prelude>
- <https://github.com/purescript/purescript-profunctor>
- <https://github.com/purescript/purescript-proxy>
- <https://github.com/purescript/purescript-random>
- <https://github.com/purescript/purescript-refs>
- <https://github.com/purescript/purescript-semirings>
- <https://github.com/hdgarrood/purescript-sequences>
- <https://github.com/purescript/purescript-sets>
- <https://github.com/purescript/purescript-st>
- <https://github.com/purescript/purescript-strings>
- <https://github.com/purescript/purescript-tailrec>
- <https://github.com/purescript/purescript-these>
- <https://github.com/purescript/purescript-transformers>
- <https://github.com/purescript/purescript-tuples>
- <https://github.com/purescript/purescript-unfoldable>
- <https://github.com/purescript-contrib/purescript-unsafe-coerce>
- <https://github.com/purescript/purescript-validation>
