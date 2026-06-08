# A WHILE interpreter in OCaml

[![CI](https://github.com/tetsuo-jp/while-C-ocaml/actions/workflows/ci.yml/badge.svg)](https://github.com/tetsuo-jp/while-C-ocaml/actions/workflows/ci.yml)

An interpreter for the **WHILE** language from Neil D. Jones,
*Computability and Complexity: From a Programming Perspective*, The MIT Press, 1997
([online version](http://www.diku.dk/~neil/comp2book2007/book-whole.pdf)).
Page references in source comments (e.g. `p.38 Definition 2.2.3`) point to this book.

WHILE is a minimal imperative language over binary trees (`nil`, atoms, and
`cons` pairs). Its core has only assignment and `while` loops, yet it is
Turing-complete — which makes it a convenient vehicle for studying
computability and complexity.

## Repository layout

```
src/core/          OCaml interpreter for the *core* WHILE language
src/desugar/       Haskell desugarer: extended WHILE → core WHILE
examples/          core WHILE programs (.while) and input values (.val)
examples/desugar/  extended WHILE example programs
web/               PHP front end that runs the interpreter
```

The toolchain is a two-stage pipeline:

```
extended WHILE --[DesugarWhile (Haskell)]--> core WHILE --[while (OCaml)]--> value
```

## The language

### Core WHILE (`src/core/While.cf`)

```
E ::= X | d | cons E E | hd E | tl E | =? E E
C ::= X := E;  |  while E do { C* }
P ::= read X; C* write Y
```

Values are binary trees: `nil`, atoms (`'a`, `'foo`, `'32`), and pairs
`(v . w)`. Anything non-`nil` counts as true.

Example (`examples/reverse.while`, list reversal):

```
read X;
  Y := nil;
  while X do {
    Y := cons (hd X) Y;
    X := tl X;
  }
write Y
```

### Extended WHILE (`src/desugar/While.cf`)

The desugarer additionally accepts, and compiles away:

| Construct | Example | Desugared by |
|---|---|---|
| conditionals | `if E then { C } else { C }` | `expandIf` |
| pattern matching | `case E1, E2 of { (H . T), Z -> C ... }` | `transCase` |
| named procedures (inlined) | `procedure p read X; ... write Y` / `Y := p X;` | `doInline` |
| logical and | `and E F` | `transAnd` |
| tree predicates | `cons? E`, `atom? E` | `transConsp` |
| blocks | `begin { C* } end` | `transBlk` |
| list notation | `('a 'b . 'c)`, `list E F`, `cons* E F G` | `transList` |
| numbers, booleans | `3`, `true`, `false` | `transNumber` |

The pipeline (`src/desugar/Desugar.hs`) applies, in order:
`transCase → transAnd → transConsp → doInline → extractMain → expandIf → transBlk → transList → transNumber`.

## Requirements

Core interpreter (OCaml):

* OCaml (ocamlc, ocamllex, ocamlyacc)
* [The BNF Converter](http://bnfc.digitalgrammars.com/) (bnfc)
* OUnit2 for the unit tests: `opam install ounit2 ocamlfind`

Desugarer (Haskell):

* GHC, happy, alex, bnfc
* libraries: `syb`, `mtl`, `array` (and `hspec` for the unit tests), e.g.

```
cd src/desugar
cabal install --lib syb mtl array hspec --package-env .
```

## Build and run

```
# core interpreter
cd src/core
make                      # builds ./while
./while ../../examples/reverse.while ../../examples/list123.val
# => ('3 . ('2 . ('1 . nil)))
./while --time ../../examples/reverse.while ../../examples/list123.val
# => 34     (time_p(d): execution steps per the book's unit-cost model, ch. 16-19)

# desugarer
cd src/desugar
make                      # builds ./DesugarWhile
./DesugarWhile ../../examples/desugar/reverse-case.while   # prints core WHILE
```

Running an extended program end to end:

```
src/desugar/DesugarWhile examples/desugar/reverse-case.while > /tmp/prog.while
src/core/while /tmp/prog.while examples/list123.val
```

### Programs as data (p.49)

`--data` prints the *programs-as-data* representation ⌜p⌝ — the program
encoded as a WHILE value, so that programs can be fed as input to other
programs (the prerequisite for universal programs / self-interpreters,
book ch. 3):

```
src/desugar/DesugarWhile --data examples/desugar/reverse-indent.while
# => (('var . ((nil . nil) . nil)) . (('semi . ...
```

Variables become `(var i)` (read variable first, output variable last),
numbers are `nil^i`, and the book's atoms `:=`, `;`, `=?` are spelled
`'asgn`, `'semi`, `'eq` (atoms are lexically alphanumeric). Empty command
sequences (e.g. desugared `skip`) are encoded as the no-op `V1 := V1`.

### Universal program / self-interpreter (pp. 50–)

`examples/desugar/universal.while` is the book's universal program `u`
written in extended WHILE — its `STEP` macro is a direct transcription of
the book's `rewrite [Cd, St]` rules into a multi-scrutinee `case`. It
satisfies ⟦u⟧(⌜p⌝.d) = ⟦p⟧(d):

```
src/desugar/DesugarWhile examples/desugar/universal.while > /tmp/u.while
src/desugar/DesugarWhile --data examples/desugar/reverse-indent.while > /tmp/rev.val
printf '(%s . %s)' "$(cat /tmp/rev.val)" "$(cat examples/list123.val)" > /tmp/pd.val
src/core/while /tmp/u.while /tmp/pd.val
# => ('3 . ('2 . ('1 . nil)))   — same as running reverse directly
```

For heavier runs build the native-code interpreter with
`make -C src/core while.opt`. The self-application
⟦u⟧(⌜u⌝.(⌜p⌝.d)) works — `u` interpreting `u` interpreting the identity
program returns the right answer in ~2 minutes — and each interpretation
layer costs a factor of ~10⁴–10⁵, a hands-on motivation for the book's
*timed universal program* (ch. 19, Hierarchy Theorem).

### Timed universal program (ch. 19)

`examples/desugar/timed-universal.while` is the book's `tt`: `u` plus a
time limit `nil^n` and clocking code that decrements it once per source
operation. On input `((⌜p⌝.d) . nil^n)` it returns `(⟦p⟧(d) . nil)` when
`time_p(d) ≤ n` and `nil` (time-limit exceeded) otherwise. The decrement
points correspond exactly to the `+1`s of the `--time` cost model, so the
boundary is sharp at `n = time_p(d)` — verified against the `--time`
oracle by `examples/desugar/test-timed-universal.sh`.

`examples/desugar/bench-efficiency.sh` then measures the interpretation
overhead `time_tt/time_p`: it converges to a constant (~2350) for
one-variable programs — *efficient* in the book's sense — but grows with
the number of variables, the `O(#vars)` access cost that confines the
efficiency theorem to the one-variable I language. Full write-up in
[docs/EFFICIENCY.md](docs/EFFICIENCY.md).

`examples/desugar/diag.while` is the diagonalization program of the
linear-time hierarchy theorem ("constant time factors *do* matter"): it
calls the timed universal program (as an inlined `procedure tu`) on
`(⌜p⌝.⌜p⌝)` with a time budget and **negates** the result. So
`diag(⌜p⌝) = false` when `⟦p⟧(⌜p⌝)` returns true within budget, and
`true` when it returns false or times out — the contradiction that
separates `TIME(a·b·n)` from `TIME(a·n)`. Demonstrated by
`examples/desugar/test-diag.sh`. The overall plan is in
[docs/TIMED-UNIVERSAL-PLAN.md](docs/TIMED-UNIVERSAL-PLAN.md).

### WHILE → one variable (the I language)

`DesugarWhile --onevar` translates a (desugared) program to an equivalent
**one-variable** program (the I language) by packing all variables into a
single list `A` and accessing `Xi` as `hd (tl^{i-1} A)` (Prop. many-one-var):

```
src/desugar/DesugarWhile --onevar examples/desugar/reverse-indent.while
# read A; A := cons A nil; A := cons (hd A) (cons nil nil); while hd A do { ... } ...
```

`examples/desugar/test-onevar.sh` checks that the one-variable version
computes the same function and uses exactly one variable. This is the
construction the book uses to turn the (multi-variable) interpreter into a
genuinely efficient universal program for I: a one-variable program is
interpreted by the universal program with a *uniform* constant overhead
(its variable access is O(1)), unlike the multi-variable case in
[docs/EFFICIENCY.md](docs/EFFICIENCY.md).

## Tests

```
# example-based smoke tests
make -C src/core test
make -C src/desugar test

# unit tests
make -C src/core unittest        # OUnit2,  32 tests over EvalWhile
make -C src/desugar unittest     # hspec,   43 tests over the desugar passes
make -C src/desugar coverage     # + HPC statement-coverage report (100%)
```

## License

GNU Affero General Public License v3.0 — see [LICENSE](LICENSE).
