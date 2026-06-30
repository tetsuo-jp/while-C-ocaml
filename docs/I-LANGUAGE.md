# The I language (one-variable WHILE) and its Turing-completeness

Jones's textbook (*Computability and Complexity*, ch. 3-4) shows that the full
WHILE language can be reduced to a **one-variable** core: the **I language**.
This repo treats I as a sub-target of WHILE, and the Agda development gives a
machine-checked account of *why one variable is enough*.

## Where the I language lives in this repo

There is **no separate I-language repository** — I is realised inside
`while-C-ocaml`:

- **`DesugarWhile --onevar`** (`src/desugar/DesugarWhile.hs`, `Desugar.hs`) compiles
  a multi-variable WHILE program to an equivalent **one-variable** program, packing
  every variable into a single list `A` (Jones's many-variable → one-variable
  construction).
- `README.md` §"WHILE → one variable (the I language)" describes the translation.
- `examples/desugar/test-onevar.sh` checks the output uses exactly one variable.
- `docs/TIMED-UNIVERSAL-PLAN.md` / `docs/EFFICIENCY.md` use the one-variable I
  language for the timed-universal / efficiency results.

On the Agda side, **`proofs/agda/UniversalInterp.agda`'s `Exp` IS the I language**:
a single variable `var` (the program's one input), the unbounded binary-tree data
domain `V` (`⟨⟩` / `_∙_`), a conditional `kifnil`, the destructors `khd`/`ktl`, the
constructor `kcons`, and `call` for recursion. Everything below is about this `Exp`.

## "One variable, yet Turing-complete" — the argument, machine-checked

The apparent paradox dissolves once you notice the data domain is **unbounded
trees**: a single variable can hold arbitrarily much structured state. The
machine-checked pieces:

1. **One variable = unboundedly many independent registers.**
   `proofs/agda/OneVarTuring.agda` packs a ℕ-indexed register file into the one
   value and proves the array axioms:
   - `get-set-eq  : getR i (setR i v s) ≡ v`
   - `get-set-neq : i ≠ j → getR i (setR j v s) ≡ getR i s`
   So the one variable simulates as many independent variables as you like — the
   "only one variable" restriction costs nothing. (This is the heart of Jones's
   many-variable → one-variable reduction, the `--onevar` pass, at the value
   level.)

2. **Unbounded iteration / recursion.**
   `Exp`'s `call` is general recursion to the (single) global program; the
   fuel-indexed interpreter `UniversalInterp.eval` makes this total *as a model*
   while leaving the object language genuinely partial (programs may diverge) —
   verified by running a real recursive `drain` program (`SelfInterp`).

3. **A universal one-variable program exists.**
   `proofs/agda/SelfInterp.agda` is a **self-interpreter `sint : Exp`** — it uses
   only `var`, yet interprets *every* encoded program ⌜p⌝ over the whole language
   (var/knil/kcons/khd/ktl/kifnil/call), verified by computation on concrete
   programs including recursion. `proofs/agda/SelfInterpRun.agda` runs the
   *encoded* self-interpreter ⌜sint⌝ through the universal machine on `(⌜p⌝ . (⌜p⌝
   . inp))` and gets `⟦p⟧(inp)` — Jones's `universal.while` picture, everything as
   data, in one variable.

Together: (1)+(2) show one-variable `Exp` matches multi-variable WHILE — Jones's
universal model — and (3) exhibits a concrete universal program written with a
single variable. Hence the I language is Turing-complete despite having one
variable; the obstacle was never the variable count, only the richness of the
data domain, which here is unbounded.

## Status / next steps

- Proven: the register-packing array axioms (`OneVarTuring`), the self-interpreter
  on concrete programs (`SelfInterp`/`SelfInterpRun`), unbounded recursion
  (`UniversalInterp`).
- Not yet a single monolithic `--safe` theorem "Exp ⊇ all computable functions":
  that would compile a fixed Turing-complete model (e.g. a 2-counter machine, or
  multi-variable WHILE) into `Exp` with a correctness proof. The pieces above are
  the constituents of that argument; the `--onevar` pass is its operational
  counterpart on real programs.
