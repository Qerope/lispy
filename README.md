# lispy

Scheme interpreter written while working through *Write Yourself a Scheme in
48 Hours*. Supports integers, strings, booleans, lists, `quote`, `if`, `cond`
and the usual arithmetic, comparison and list primitives.

    cabal build
    cabal run lispy             # REPL
    cabal run lispy '(+ 2 3)'   # evaluate one expression

No variables or functions yet; the `env` branch has the start of that.
