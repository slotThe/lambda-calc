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

## Examples

This implementation is stream-focused—that means the evaluator has no
"memory" of what came before it.  This would be superfluous anyways, as
there is no way to define functions other than lambdas.

- Primitive types are:

  - Integers: `1`, `100`, `42`, …
  - Strings: `"Vi Vi Vi, the editor of the beast."`
  - Booleans: `#t` and `#f`

- For lambdas both `λ` and `\` can be used

  ```
    >>> (λa. a) 3
    3

    >>> (\a. a + 1) 41
    42
  ```

  Likewise, instead of `.` you can also use `->` or `→` to differentiate
  the lambda's head from its body.

- Multiple parameters to a lambda can be specified by simply separating
  them by spaces—this is just syntactic sugar for multiple chained
  lambdas

  ```
    >>> (\ a b c -> a + b*c)
    λa. λb. λc. a + b * c
  ```

- Function application is denoted by whitespace; it has higher
  precedence than binary operations

  ```
    >>> (\f a. f a + 30) (\a -> a * 10) 7
    100
  ```

- You can query the type of an expression by prefixing it with `:t`

  ```
    >>> :t \f g a. f (g a a)
    (b → c) → (a → a → b) → a → c

    >>> :t \f a. f a + 42
    (a → Int) → a → Int
  ```
