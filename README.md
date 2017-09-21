GHC.Hotswap
===========

Demonstrates how to build an application that can hot-swap parts of itself at runtime, using GHC. This is a standalone illustration of the technology used in production in the [Sigma system at Facebook](https://code.facebook.com/posts/745068642270222/fighting-spam-with-haskell/).

There are a handful of Haskell packages here:

 * [ghc-hotswap](ghc-hotswap): Core library for swapping shared objects safely
 * [ghc-hotswap-demo](ghc-hotswap-demo): Demo program that shows up swapping
 * [ghc-hotswap-so](ghc-hotswap-so): Sample cabal project for building a shared object
 * [ghc-hotswap-types](ghc-hotswap-types): Common libraries between the `so` and `demo` packages

# ghc-hotswap

Shared object swapping goodness.

# Demo

Quick Start
-----------

- cd ghc-hotswap-types
- cabal configure
- cabal install

- cd ghc-hotswap
- cabal configure
- cabal install

- cd ghc-hotswap-so
- cabal configure --enable-library-for-ghci
- cabal build

(Note the file path of `dist/build/HSghc-hotswap-so(...).o`)

- cd ghc-hotswap-demo
- cabal configure
- cabal build
- cabal run <path of first .o file>

## ghc-hotswap-types

Example library for common types between the main binary and shared object. The object code for this library will end up in the final binary, but the shared object needs to know what types to generate at the API boundary.

No special configuration for this library

- cabal configure
- cabal install

## ghc-hotswap

General library for loading and consecutively updating shared objects in a concurrency-safe manner.

No special configuration for this library

- cabal configure
- cabal install

## ghc-hotswap-so

Example shared object library. Constains:

  * `Handles` [module](ghc-hotswap-so/SO/Handles.hs) defining a function that returns a StablePtr to our expected API object and exports a c-symbol for it.
  * `MyCode` [module](ghc-hotswap-so/SO/MyCode.hs) as an example of complex library code that can be updated quickly

The main purpose is to generate an object file that another binary will know how to dynamically open. The `--enable-library-for-ghci` cabal flag does this for us.

- cabal configure --enable-library-for-ghci
- cabal build

The file we care about gets placed in `ghc-hotswap-so/dist/build/` with a filename prefix like `HSghc-hotswap-so` and extension `.o`. The path to this file is important (or copy it to a nicer location for yourself) as you'll need it later.

## ghc-hotswap-demo

Demo executable for loading + unloading shared objects on the fly.

Note the configuration set [in ghc-hotswap-demo.cabal](ghc-hotswap-demo/ghc-hotswap-demo.cabal). It does not depend on `ghc-hotswap-so` and adds `-rdynamic` and `-fwhole-archive-hs-libs as GHC flags.

- cabal configure
- cabal run <path-of-first-.o>

The program loads in the first shared object and periodically prints some information. It's waiting for input on stdin for the path of the next shared object to use.

  * Go back to ghc-hotswap-so
  * Edit `SO/Handles.hs` or `SO/MyCode.hs` to do as you want
  * Rebuild the object file (`cabal build`)
  * Copy the desired `.o` file somewhere friendlier
  * Send the full path of the `.o` to stdin of the demo program
  * See the output of the program change live
  * ???
  * Profit!
