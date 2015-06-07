# bytestring-lexing [![Hackage version](https://img.shields.io/hackage/v/bytestring-lexing.svg?style=flat)](https://hackage.haskell.org/package/bytestring-lexing) [![TravisCI Build Status](https://img.shields.io/travis/haskell/bytestring-lexing.svg?style=flat)](https://travis-ci.org/haskell/bytestring-lexing) [![CircleCI Build Status](https://circleci.com/gh/wrengr/bytestring-lexing.svg?style=shield&circle-token=:circle-token)](https://circleci.com/gh/wrengr/bytestring-lexing)

The bytestring-lexing package offers extremely efficient `ByteString`
parsers for some common lexemes: namely integral and fractional
numbers. In addition, it provides efficient serializers for (some
of) the formats it parses.

As of version 0.3.0, bytestring-lexing offers the best-in-show
parsers for integral values. (According to the Warp web server's
benchmark of parsing the Content-Length field of HTTP headers.) And
as of version 0.5.0 it offers (to my knowledge) the best-in-show
parser for fractional/floating numbers. A record of these benchmarks
can be found
[here](http://code.haskell.org/~wren/bytestring-lexing/bench/html)

Note that the GitHub repository is just a clone of [the Darcs
repo](http://code.haskell.org/~wren/bytestring-lexing/). I'm testing
out whether to switch things over to GitHub in order to use TravisCI,
and an official ticket tracker, etc.


## Install

This is a simple package and should be easy to install. You should
be able to use one of the following standard methods to install it.

    -- With cabal-install and without the source:
    $> cabal install bytestring-lexing
    
    -- With cabal-install and with the source already:
    $> cd bytestring-lexing
    $> cabal install
    
    -- Without cabal-install, but with the source already:
    $> cd bytestring-lexing
    $> runhaskell Setup.hs configure --user --enable-tests
    $> runhaskell Setup.hs build
    $> runhaskell Setup.hs test
    $> runhaskell Setup.hs haddock --hyperlink-source
    $> runhaskell Setup.hs copy
    $> runhaskell Setup.hs register

The test step is optional and currently does nothing. The Haddock
step is also optional.


## Portability

An attempt has been made to keep this library portable. However,
the `decimalPrecision` function in `Data.ByteString.Lex.Fractional`
requires ScopedTypeVariables for efficiency. If your compiler does
not support ScopedTypeVariables, this should be easy enough to fix.
Contact the maintainer if this is an issue for you.


## Changes: Version 0.5.0 (2015-05-06) vs 0.4.3 (2013-03-21)

I've completely overhauled the parsers for fractional numbers.

The old `Data.ByteString.Lex.Double` and `Data.ByteString.Lex.Lazy.Double`
modules have been removed, as has their reliance on Alex as a build
tool. I know some users were reluctant to use bytestring-lexing
because of that dependency, and forked their own version of
bytestring-lexing-0.3.0's integral parsers. This is no longer an
issue, and those users are requested to switch over to using
bytestring-lexing.

The old modules are replaced by the new `Data.ByteString.Lex.Fractional`
module. This module provides two variants of the primary parsers.
The `readDecimal` and `readExponential` functions are very simple
and should suffice for most users' needs. The `readDecimalLimited`
and `readExponentialLimited` are variants which take an argument
specifying the desired precision limit (in decimal digits). With
care, the limited-precision parsers can perform far more efficiently
than the unlimited-precision parsers. Performance aside, they can
also be used to intentionally restrict the precision of your program's
inputs.


## Benchmarks: Version 0.5.0 (2015-05-06)

The Criterion output of the benchmark discussed below, [is available
here](http://code.haskell.org/~wren/bytestring-lexing/bench/html/readExponential-0.5.0_ereshkigal.html).
The main competitors we compare against are the previous version
of bytestring-lexing (which already surpassed text and
attoparsec/scientific) and bytestring-read which was the previous
best-in-show.

The unlimited-precision parsers provide 3.3x to 3.9x speedup over
the `readDouble` function from bytestring-lexing-0.4.3.3, as well
as being polymorphic over all `Fractional` values. For `Float`/`Double`:
these functions have essentially the same performance as bytestring-read
on reasonable inputs (1.07x to 0.89x), but for inputs which have
far more precision than `Float`/`Double` can handle these functions
are much slower than bytestring-read (0.30x 'speedup'). However,
for `Rational`: these functions provide 1.26x to 1.96x speedup
compared to bytestring-read.

The limited-precision parsers do even better, but require some care
to use properly. For types with infinite precision (e.g., `Rational`)
we can pass in an 'infinite' limit by passing the length of the
input string plus one. For `Rational`: doing so provides 1.5x speedup
over the unlimited-precision parsers (and 1.9x to 3x speedup over
bytestring-read), because we can avoid intermediate renormalizations.
Whether other unlimited precision types would see the same benefit
remains an open question.

For types with inherently limited precision (e.g., `Float`/`Double`),
we could either pass in an 'infinite' limit or we could pass in the
actual inherent limit. For types with inherently limited precision,
passing in an 'infinite' limit degrades performance compared to the
unlimited-precision parsers (0.51x to 0.8x 'speedup'). Whereas,
passing in the actual inherent limit gives 1.3x to 4.5x speedup
over the unlimited-precision parsers. They also provide 1.2x to
1.4x speedup over bytestring-read; for a total of 5.1x to 14.4x
speedup over bytestring-lexing-0.4.3.3!


## Links

* [Website](http://cl.indiana.edu/~wren/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/bytestring-lexing)
* [Darcs](http://code.haskell.org/~wren/bytestring-lexing)
* [GitHub (clone)](https://github.com/wrengr/bytestring-lexing)
* [Haddock (Darcs version)
    ](http://code.haskell.org/~wren/bytestring-lexing/dist/doc/html/bytestring-lexing)
