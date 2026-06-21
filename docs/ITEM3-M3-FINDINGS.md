# item-3 M3 findings — why M2's prototype doesn't extend, and the redesign

Worktree `item3-m1`. M2 (commit b587874) folds a single self-loop and makes
`usi×rev` Jones-optimal in control (#while=1, #eq=0, correct). Attempting M3
(universal loop-free + nested dynamic control) on top of the M2 prototype hit
hard limits; this records them so the redesign is direct. The M3 patches were
**reverted** (they regressed M2/the corpus); the worktree stays at verified M2.

## What M2's prototype is

`drive`/`drive_dyn_if` (COGEN_MEMO): explicit continuation `k`; `while` unfolded
into `k`; a dynamic-`if` is treated as a *candidate self-loop* — push its config
`(σ_s, k)` on `loop_stack`, drive the then-branch + continuation, and if it
re-reaches THIS config (a `Fold` exception caught by the same handler) emit a
residual `while`; else it's "not a loop".

## Why it doesn't extend (measured)

1. **Non-loop dynamic-`if` is wrong.** The "not a loop" path drove both branches
   to completion; on `int_if` it produced `(nil . 'none)` (wrong). Even after
   fixing the post-trial store restore, it stayed wrong — the both-branches drive
   does not correctly merge the two residual stores (the proven `residualize_if`
   does a real merge with per-branch fix-ups; `drive` reimplemented it incompletely).
2. **Bailing on non-loop breaks M2.** Replacing the non-loop path with "bail to
   spec" broke `usi×rev`: the loop's **entry** `'dowh` is itself a *non-loop*
   dynamic-`if` (executed once; the actual back-edge is a *later*, steady-state
   `'dowh` whose config differs from the entry's by the first-iteration transient).
   So "fold self-loops, bail on everything else" cannot even handle a single
   subject loop — the entry and the steady state are different configs.
3. **Universal raises an uncaught `Fold`.** A back-edge can target a *non-immediate*
   ancestor; the exception scheme only unwinds to the immediate handler, so a
   `Fold` for an outer config escapes → crash (now guarded by a top-level fallback,
   but that just degrades to spec, not loop-free).
4. **Nested control (`int_nest`) blows up.** Both-branches driving duplicates the
   continuation per dynamic-`if` → exponential; only the step budget bounds it
   (slow), then bails.

## The redesign (true M3): a global memo table, not an ancestor stack

Replace the exception/ancestor-stack scheme with standard memoized supercompilation:

- **Global memo** `seen : config ⇀ name`, where `config = (σ_s, k)` with `σ_s` the
  spine-preserving store projection and `k` the continuation. Every driving point
  is looked up; a hit is a back-edge to *that* `name` (any ancestor or sibling),
  not just the immediate one.
- **Residual as labelled blocks**, then reconstruct structured WHILE: a single
  back-edge to a config dominating the body → a `while`; the loop ENTRY (transient)
  is just straight-line code *before* the loop (so M2-finding #2 dissolves — entry
  and steady state are distinct memo entries, the steady one carries the back-edge).
  Multiple/irreducible back-edges → a `pc`-dispatch `while` (correct, one loop) as
  the fallback (M1 strategy (a) in ITEM3-DESIGN.md).
- **Non-loop dynamic-`if`**: emit a residual `if`, drive each branch with the SAME
  global memo (so shared continuations are not re-driven → no exponential blowup),
  and **merge** stores exactly as the proven `residualize_if`/`mix.while`'s
  dynamic-if merge does (per-branch fix-up assignments).
- **Generalization**: when a config does not recur but keeps growing, generalize
  via the homeomorphic-embedding whistle (RELATED-WORK §C5(i)) instead of the crude
  budget; this is the item-2 whistle, which (per the earlier finding) only has
  teeth at the configuration level — i.e. exactly here.

## Takeaway

M2 proves the core mechanism (fold a subject loop, keep the interpreter code
pointer static so dispatch unrolls → Jones-optimal control). M3 is not a patch on
the prototype but the standard memoized supercompiler (global config table +
structured-loop reconstruction + real store merge), which is a multi-session
build. M2 stands as the milestone; the global-memo driver is the next.
