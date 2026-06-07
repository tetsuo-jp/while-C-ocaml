# Plan: efficient timed universal program & the linear-time hierarchy (ch. 19)

Goal: realize the *timed universal program* `tu` and demonstrate the
**linear-time hierarchy theorem** ("constant time factors *do* matter")
from Jones ch. 19, on top of this repository.

This document is the design/plan only — no code is committed by it.

## What the book asks for

- **Timing model** (Def. "Running times of WHILE programs", §"measuring time"):
  one time unit per operation/test on data.
  `T[X]=T[d]=1`, `T[hd E]=T[tl E]=1+T[E]`, `T[cons E F]=1+T[E]+T[F]`;
  `X:=E` costs `T[E]+1`; `C;D` adds; `while E do C` costs `T[E]+1`
  (guard nil) or `T[E]+ (cost of C; while) + 1` (guard non-nil).
- **Timed universal program** `tu` (Def. `timed-universal`): for all p, d, n≥1
  - if `time_p(d) ≤ n` then `⟦tu⟧(p.d.nil^n) = (⟦p⟧(d) . nil)`
  - if `time_p(d) > n` then `⟦tu⟧(p.d.nil^n) = nil`  ("time limit exceeded")
- **Efficient** `tu` (Def. `efficient-timed-universal`): ∃k ∀p,d,n
  `time_tu((p.d).nil^n) ≤ k · min(n, time_p(d))`.
- **Program `tt`** (Fig. `fig-tu`): `u1var` + a `Cntr := nil^n` time limit +
  clocking code that, each time one source operation completes, decrements
  `Cntr` and tests it; when it hits zero, set `Cd := nil; X := nil`.
  The decrement fires when the top opcode is in
  `{quote, var, do_hd, do_tl, do_cons, do_asgn, do_while}`.
- **Hierarchy theorem** (Thm. `thm-constant-hierarchy`): ∃b ∀a≥1, there is a
  set in `TIME^I(a·b·n)` not in `TIME^I(a·n)`, via the diagonal program
  `diag` (Fig. `fig-diagonal-program`) that runs `tu` on `(X.X)` with time
  bound `nil^{a·|X|}` and flips the answer.

Key fact we exploit: the cost model's `+1`s correspond **exactly** to the
completion opcodes above, so a `Cntr` decremented on those opcodes counts
source time units precisely. Our `universal.while` already uses the matching
internal opcodes `'dohd 'dotl 'docons 'doeq 'doasgn 'dowh` plus the leaf cases
`'quote`/`'var` — the decrement set maps one-to-one.

## Scope decision: I vs. full WHILE

The book proves `tu` **efficient** only for the **I language** (one-variable
WHILE), because multi-variable `lookup`/`update` cost O(#vars) per access —
exactly the non-constant overhead noted after Prop. `thm-while-int-timing`.
Our current `universal.while` is the *multi-variable* `u`, so it is a correct
*timed* universal program but **not efficient** for arbitrary programs.

We therefore split the work: get the timed semantics first (works for full
WHILE), then address efficiency on the one-variable path the theorem needs.

## Phased plan

### Phase 0 — Timing instrumentation in the core interpreter  (foundation)
- Add a step counter to `src/core/EvalWhile.ml` implementing `T` and
  `⊢^time` literally, exposing `timeProc : proc -> valT -> int` alongside
  `evalProc`.
- New CLI flag `src/core/while --time prog data` prints `time_p(d)`.
- This is independently useful and becomes the **reference oracle** for
  testing `tu` (compare `tu`'s success/timeout against the true step count).
- Tests: extend `EvalWhileTest.ml` — `T[cons(hd X)(tl X)]`, a while loop's
  per-iteration cost, reverse's total = closed form in list length.
- Deliverable: `time_p(d)` measurable; ~40 OUnit cases.

### Phase 1 — Timed universal program `tt` for full WHILE  ✅ DONE
- `examples/desugar/timed-universal.while`: `universal.while` + a third input
  `nil^n` read into `Cntr`, with each of the 8 completion arms guarded by
  `if Cntr then { Cntr := tl Cntr; <op> } else { stop; Timeout }`; the loop
  exits to `(result . nil)` on success or `nil` on timeout.
- Verified against the Phase-0 oracle at the exact boundary
  (`n = time-1` → `nil`, `n = time` → `(result.nil)`) by
  `examples/desugar/test-timed-universal.sh`, wired into `make test` and CI.
- Result: a correct (not-yet-efficient) timed `tu` for full WHILE.

### Phase 2 — Demonstrate efficiency on one-variable programs  ✅ DONE
- `examples/desugar/bench-efficiency.sh` measures `time_tt/time_p` with the
  `--time` oracle. (A) one-variable family → ratio converges to k ≈ 2350
  (efficient: `time_tt = k·time_p + c`). (B) k-variable family → ratio grows
  monotonically with #vars (the `O(#vars)` access cost).
- Results written up in `docs/EFFICIENCY.md`; this is the empirical evidence
  for Lemma `thm-timed-univ-pgm` and shows why efficiency is an I-language
  (one-variable) property.

### Phase 3 — The diagonal program & hierarchy separation  (stretch)
- `examples/desugar/diag.while` per Fig. `fig-diagonal-program`:
  `Timebound := nil^{a·|X|}` (needs a length/`|·|` helper in extended WHILE),
  `Arg := cons (cons X X) Timebound`, `X := tu Arg`, flip on `hd X`.
- Demonstrate the separation concretely: exhibit small programs that
  `diag` (running in `a·b·n`) classifies but no `a·n` program can, i.e.
  reproduce the diagonal contradiction on finite cases (illustration, not a
  machine-checked proof).
- True faithfulness needs the **one-variable I translation**
  (Prop. `many-one-var`, the "WHILE → one-variable" construction): a larger
  sub-project. Decide then whether to (a) implement that translation, or
  (b) keep the demonstration at the multi-variable level and state the gap.
- Deliverable: runnable `diag`, a written walkthrough of the separation.

## Risks / open questions
- **Efficiency rigor**: a runnable artifact illustrates but does not *prove*
  the linear bound. Phase 2 gives empirical `k`; a proof stays on paper.
- **One-variable translation** (Prop. `many-one-var`) is the biggest unknown;
  it is its own TODO and gates a fully faithful Phase 3.
- **`=?` vs nil-tests**: the book, from §"only atomic comparisons" on, assumes
  comparisons are against `nil` only (unit cost). Our `=?` is structural; for
  timing fidelity we either (a) keep `=?` as unit cost on the result of one
  comparison, or (b) restrict timed examples to the nil-test fragment. Note
  this in the timing model docs.
- **Cost of `nil^n` input**: building the `nil^n` time bound is itself O(n);
  `diag` accounts for this (`c·a·|X|`). Keep it outside the measured `tu`
  region.

## Suggested order
Phase 0 → Phase 1 (these two give a usable, tested timed `tu` and the timing
tool) → Phase 2 (the efficiency evidence) → Phase 3 (stretch, may spawn the
one-variable-translation TODO). Phases 0–2 are self-contained and CI-able;
Phase 3 is a good student project.
