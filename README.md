# lambda-calc

The obligatory simply typed lambda calculus interpreter.

## Installation and Usage

Build with `stack build` (or install via `stack install`).  By default,
the executable contains a simple repl.  For more ergonomic usage, you
can use it with [rlwrap]:

``` console
  $ stack build
  $ rlwrap stack exec -- lambda-calc
```

[rlwrap]: https://github.com/hanslub42/rlwrap
