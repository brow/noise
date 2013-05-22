# Noise

Noise is a concise, friendly language for graphic design that translates directly to [SVG 1.1](http://www.w3.org/TR/SVG11/). You can learn more about the language at [its webpage](http://tombrow.com/noise).

This project is an implementation of Noise written in [Haskell](http://haskell.org). It includes an interpreter and a library of modules that you can use to write your own interpreter.

## Installation

First, install the [Haskell Platform](http://www.haskell.org/platform/). Then:

    cabal install noise

To test the installation, invoke `noise`:

    $ noise --help
    Usage: noise [file]
      -h  --help  Print this help text.

## Usage

`noise` reads Noise code from standard input and writes SVG to standard output:

    echo "shape.circle(10,10,10,fill:color.red)" | noise > circle.svg

It can also read from a file:

    echo "shape.rectangle(0,0,10,10,fill:color.blue)" > rectangle.noise
    noise rectangle.noise > rectangle.svg

Use `convert` from the [ImageMagick](http://www.imagemagick.org/) package to write other image formats:

    echo "shape.circle(10,10,10,fill:color.green)" | noise | convert -size 20x20 svg:- circle.png

## Development

I recommend using `cabal-dev` to maintain a sandboxed build environment. If you don't have it already:

    cabal install cabal-dev

Then do this:

    git clone git@github.com:brow/noise.git
    cd noise
    cabal-dev install-deps --enable-tests
    cabal-dev configure --enable-tests

After performing the above setup once, you can build and test `noise` like so:

    cabal-dev build && cabal-dev test

For a more detailed and colorful test report, try this:

    cabal-dev build && ./dist/build/test/test
