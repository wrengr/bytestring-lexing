# bytestring-lexing [![Hackage version](https://img.shields.io/hackage/v/bytestring-lexing.svg?style=flat)](https://hackage.haskell.org/package/bytestring-lexing) [![Build Status](https://img.shields.io/travis/haskell/bytestring-lexing.svg?style=flat)](https://travis-ci.org/haskell/bytestring-lexing)

The bytestring-lexing package offers extremely efficient `ByteString`
parsers for some common lexemes: namely integral and fractional
numbers. In addition, it provides efficient serializers for (some
of) the formats it parses.

As of version 0.3.0, bytestring-lexing offers the best-in-show
parsers for integral values. (According to the Warp web server's
benchmark of parsing the Content-Length field of HTTP headers.) And
as of version 0.5.0 it offers (to my knowledge) the best-in-show
parser for fractional/floating numbers.

Note that this GitHub repository is just a clone of [the Darcs repo](http://code.haskell.org/~wren/bytestring-lexing/). I'm testing out whether to switch things over to GitHub in order to use TravisCI, and an official ticket tracker, etc. 


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


## Links

* [Website](http://cl.indiana.edu/~wren/)
* [Blog](http://winterkoninkje.dreamwidth.org/)
* [Twitter](https://twitter.com/wrengr)
* [Hackage](http://hackage.haskell.org/package/bytestring-lexing)
* [Darcs](http://code.haskell.org/~wren/bytestring-lexing)
* [GitHub (clone)](https://github.com/wrengr/bytestring-lexing)
* [Haddock (Darcs version)
    ](http://code.haskell.org/~wren/bytestring-lexing/dist/doc/html/bytestring-lexing)