# Runnable examples

Each program is ordinary OCaml source. `mini-liquid` asks the OCaml compiler
for its Typedtree, converts the supported subset to its thin AST, then
generates Liquid-Type constraints and validates them with Z3.

Run an example from the repository root:

```sh
dune exec ./bin/main.exe -- max
dune exec ./bin/main.exe -- voting
dune exec ./bin/main.exe -- grade
dune exec ./bin/main.exe -- sum
dune exec ./bin/main.exe -- fibonacci
```

| Example | Intended inferred fact |
| --- | --- |
| `max.ml` | result is at least both inputs |
| `voting.ml` | the integer result code is non-negative |
| `grade.ml` | the integer result code is non-negative |
| `sum.ml` | non-negative input; result is non-negative and at least the input |
| `fibonacci.ml` | non-negative input and result |

## Current boundaries

The supported subset has integer literals, `+`, `-`, simple `<`/`>` guards,
conditionals, non-recursive local `let`, and unary recursion. The following
are intentionally not runnable examples yet:

- FizzBuzz: needs modulo, equality checks, and strings.
- Kaprekar sequence: needs digit extraction, division/modulo, multiplication,
  and multi-value state.
- Collatz sequence: needs modulo/parity and multiplication.
- Project Euler 6: needs multiplication for squares and usually multiple
  accumulators or division-based formula arithmetic.
