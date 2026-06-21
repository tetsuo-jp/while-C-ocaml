# item-3 M1 — root cause (usi×rev) and the coupled fix

Worktree `item3-m1`. Captures the diagnosis that makes M1 implementable. See
`docs/ITEM3-DESIGN.md` for the overall plan; this file is the precise mechanism.

## Reproduce

```
sh examples/desugar/test-item3-m1.sh        # RED target (usi×rev, universal×swap)
COGEN_TRACE=1 src/core/cogen --online usi.data rev.data >/dev/null   # residualization events
```
Baseline (2026-06-22): usi×rev = 87030 B, #while=1, #eq=1; universal×swap = 210919 B, #while=1, #eq=1.

## Root cause (usi×rev)

rev is `read X; Y:=nil; while X do { Y:=cons (hd X) Y; X:=tl X }; write Y` — one
loop, guard `X` dynamic. In `usi.while` a subject `while` is handled by the
`'dowh` marker (usi.while:111-126):

```
('dowh . (WH . Cr)) -> DD := hd St; St := tl St;
   if DD then { C1 := hd(tl(tl WH)); Cd := cons C1 (cons WH Cr); }   (* loop again *)
        else { Cd := Cr; }                                            (* exit *)
```

`DD` is the guard value = **dynamic** ⇒ `if DD` residualizes (`residualize_if`).
In **both** branches `Cd` is set to a **statically-known** value (`C1`, `WH`, `Cr`
are all static program pieces). Yet `residualize_if` dynamicizes `Cd` because the
**monovariant BTA** (`bget`) classifies `Cd` as `D` (it is assigned under dynamic
control — the `'dowh`'s `if DD`), so `escaping_set`/the `residualize_if` esc set
includes it. Once `Cd := Dv (var Cd)`, the next iteration's `case Cd of` (the main
`while Cd` dispatch) is over a **dynamic** value ⇒ it residualizes instead of
unrolling ⇒ the interpreter step survives in the residual (`#eq=1`, 87 KB).

Contrast `drain.while` (Jones-optimal today): its inner dispatch register is
re-initialised `Cd := Body` at the body top, so it is **not live-in**, and the
liveness-aware `escaping_set` already keeps it static. usi's `Cd` *is* live-in
across the `'dowh` cycle (read by `case Cd of` before being rebuilt), so liveness
alone cannot save it — its value is loop-invariant in *shape* but the analysis
can't see that.

## The coupled fix (this is M1, not separable)

1. **Value-driven escaping set.** In `residualize_if` / `residualize_while`, do not
   dynamicize a var whose specialized value is **statically known and equal across
   both branches / loop-head iterations** (a merge, à la `mix.while`'s dynamic-if
   store merge). Then `Cd` stays a static `Cv` ⇒ the main dispatch unrolls.
2. **Configuration memoization (the reason 1 alone fails).** Narrowing the esc set
   makes each distinct static `Cd` spawn fresh specialization — the documented
   *combinatorial blow-up* (`residualize_if` comment, FUTAMURA "Finding"). Bound it
   with a memo table keyed on the configuration `(pp, σ_s)` where `pp` = the static
   `Cd` value (the residual program point) and `σ_s` = the static projection of the
   store (spine-preserving, dynamic leaves ⊥-abstracted). A repeated config is not
   re-specialized; the back-edge to it is emitted as the **subject's own loop**
   (`'dowh`'s `then` re-pushing `WH` is exactly the back-edge) — folding
   (Sørensen–Glück–Jones), so the residual loop = rev's `while`, `#while=1, #eq=0`.

So the `'dowh` then-branch (`Cd := cons C1 (cons WH Cr)`) is the back-edge: when we
re-reach the same `(Cd=⌜while-body⌝, σ_s)` configuration, emit a residual
`while <lift guard> do <body-once>` instead of unrolling, with the body's `Cd`
kept static so its dispatch unrolls.

## Implementation order (each validated; correctness never regresses)

- **M1a** — memo table + worklist keyed on `(pp=Cd-value, σ_s)`; on the `'dowh`/main
  loop, detect the repeated config and emit a single residual `while` (fold). Keep
  the unroll budget as the finiteness backstop until the memo guarantees it.
- **M1b** — value-driven esc (merge) so `Cd` stays static inside the loop body
  ⇒ dispatch unrolls ⇒ `#eq→0`. Witness: `usi×rev` test goes GREEN.
- **M2/M3** — same machinery makes `universal×swap` loop-free (`#while→0`); revisit
  the multi-scrutinee `case Cd,St of` with memo in place.
- **G1** — Agda congruence proof of the memoized specializer (rwhile `RWhileAVSound`
  template).

## Risks
- Termination: the memo must guarantee finiteness of the configuration set
  (HEm whistle on `(pp, σ_s)` per `docs/RELATED-WORK.md` §C5(i)); keep the budget
  until proven.
- Soundness of the merge + self-ref slots under the now-static `Cd`
  (`SpinePreservingEmit.cross-unsound`): the back-edge config equality must compare
  σ_s up to self-reference, not raw slots.
