# Noise
[Noise](http://tombrow.com/noise) is a concise, friendly language for graphic design that translates directly to [SVG 1.1](http://www.w3.org/TR/SVG11/). This project is an implementation of Noise, written in Haskell. It includes an interpreter and a library of modules that you can use to write your own interpreter.

## Installation
First, install the [Haskell Platform](http://www.haskell.org/platform/) and ensure that . Then:
    
    git clone git@github.com:brow/noise.git
    cd noise
    cabal install
    
Let's make sure it worked:

    $ noise --help
    Usage: noise [file]
      -h  --help  Print this help text.
