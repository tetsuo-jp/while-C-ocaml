# mix.while — machine-checked design specs (Agda)

Small, `--safe` Agda models that pin down the design decisions of the partial
evaluator [`examples/desugar/mix.while`](../../examples/desugar/mix.while) and
its road to the Futamura projections. They are *design specs* (à la rwhile's
`proofs/agda/RWhileRevProj2BT.agda`): they prove, in a minimal core, WHY a
particular choice in `mix.while` is the correct one, so the WHILE implementation
can be written/changed against a checked result.

Requires Agda + `standard-library` (this machine: Agda 2.8.0). Typecheck all
modules at once with `sh proofs/agda/check.sh` (also run by
`examples/desugar/run-all.sh` when `agda` is on PATH), or individually:

```
cd proofs/agda
agda --safe MixBindingTime.agda
agda --safe MixEmitModel.agda
agda --safe MixPatRead.agda
agda --safe MixMerge.agda
agda --safe RevSpecGarbage.agda
```

Both check with no postulates / holes / `--allow-unsolved` (`--safe`).

## Modules

### `MixBindingTime.agda` — MKAV / binding-time correctness (backs current code)

Non-reversible analogue of rwhile's `RWhileRevProj2BT.agda`, matching
`mix.while`'s input builder `if =? BT 'S then …('S.Sval)… else …('D.…)…`.

- `fp1-ok` — when the input half is genuinely static, the binding-time-aware
  builder agrees with an unconditional-`'S` one (so fp1 is correct either way);
- `fp2-mistags` — under self-application the input half is **dynamic**, and an
  unconditional-`'S` builder **mis-tags** it (`¬ (… ≡ …)`);
- `buggy-ignores-input` / `correct-uses-input` — the consequence: an over-static
  residual bakes a constant and ignores its runtime input, the BT-aware residual
  uses it;
- `fp1-static-bakes` / `fp1-dynamic-residual` — fp1 correctness of the BT-aware
  builder for both binding times.

⟹ justifies the `if =? BT 'S` (MKAV) test that `mix.while` already has — and
records that it is *necessary but not sufficient* for fp2 (next module).

### `MixEmitModel.agda` — emit-vs-fold soundness (backs gap #1)

Design spec for **dynamic-`while` residualisation** (gap #1 in
[`docs/FUTAMURA.md`](../../docs/FUTAMURA.md)). Models a runtime store and
residual code that references variables, and proves:

- `fold-tracks-new` / `fold-unsound` — `mix.while`'s current **fold** model
  (a dynamic slot is code referencing *another* variable, e.g. `X ↦ (var In)`)
  is **corrupted** when `DYNAMICIZE-ALL` re-materialises that variable
  (`In := w`): the slot then reads the new `In`, not the intended old value;
- `emit-sound` — the **emit-on-dynamic-assignment** model (emit `(var k) := …`
  and set the slot to the self-reference `(var k)`) is **stable** under updates
  to other variables, so it reads exactly the materialised value.

⟹ the dynamic-loop work must switch `'doasgn` to the emit model (self-referential
dynamic slots), not the current cross-referencing fold.

### `MixPatRead.agda` — why MKAV is insufficient; the read must be BT-aware (gap #2)

Design spec for **PAT-READ-ITER** (gap #2). Composes with `MixBindingTime`:
even with a binding-time-aware *builder*, the **read** of the partially-static
input (`avhd` of `('C.(('S.s).·))`) returns `('S.s)` regardless of whether `s`'s
origin is dynamic-under-self-application.

- `read-ignores-origin` — the naive read commits `s` static for either origin;
- `read-mistags` — when the origin is dynamic (fp2) the naive read differs from
  the binding-time-correct read;
- `naive-bakes` / `correct-uses` — downstream a conditional on the naive read
  bakes a branch (ignores its runtime input), one on the origin-aware read uses
  it.

⟹ the read of a partial-static input must consult the static half's *origin*
binding time and residualise when dynamic (rwhile's depth-general read).

Note: with gap #1's dynamic-loop residualisation in place, fp2 in `mix.while`
turned out **correct** (`comp2` is a large *interpreting* compiler that re-runs
the `op`-dependent work at its run time). So `MixPatRead` now backs an
**optimality** improvement (shrinking `comp2`/`comp3` to a small, non-
interpreting compiler), not a correctness fix.

### `MixMerge.agda` — dynamic-`if` store merge correctness (backs `'doif`)

Models one slot of the dynamic-`if` merge and proves `merge-correct` (with both
per-branch fix-ups, the post-`if` value equals the value the taken branch
computed) and `merge-needs-both` / `merge-buggy-wrong` (dropping the else fix-up
makes a divergent slot read a stale placeholder on the else branch).

### `BTAWiden.agda` — binding-time widening: sound + terminating (backs cogen --online)

Design spec for online-termination control (item 1). The binding-time domain
`BT = S | D | C BT BT` has infinite ascending chains — a static-control
accumulator `Acc := cons D Acc` grows `C(D, C(D, …))` unboundedly, so the naive
BTA fixpoint diverges (cogen hung on `count.while`). Models the information order
`⊑` (S least, D greatest, C pointwise) and proves `widen` (`Cogen.ml`) is both
**sound** — `widen-sound : t ⊑ widen d t`, so capping the C-depth and normalizing
`C D D → D` only moves up the lattice (the analysis stays a safe
over-approximation) — and **depth-bounded** — `depth-widen : depth (widen d t) ≤ d`,
so the widened domain is finite and the fixpoint terminates.

### `SpinePreservingEmit.agda` — spine-preserving emit: faithful + sound (backs C5)

Design spec for the spine-preserving-emit technique of `cogen --online` (C5). When
an emitted assignment's value has a static cons spine but dynamic leaves, the slot
is stored as that spine with each leaf a **self-reference** (`hd`/`tl` path) into
the slot's own variable. Models the slot (`buildSlot`/`evalSlot`) and proves
`readback` (the spine-of-self-refs reconstructs the value — spine preservation is
faithful) and `stable` (the slot references only its own variable, so
re-materialising any other variable cannot change it — emit-model soundness for the
partially-static spine case), with `cross-unsound` as the contrast (a
cross-reference slot *would* change). Complements `MixEmitModel` (the scalar case).

### `MemoFold.agda` — item-3 M2 configuration-memoization fold soundness (backs C5)

Design spec for the M2 driver (`Cogen.ml` `drive`/`drive_dyn_if`, gated
`COGEN_MEMO`). The driver unfolds the self-interpreter's loop and, on a back-edge
(the subject loop iterating), **folds** it into a residual `while g do body`. Models
a loop as a guard `g` and a step over an abstract state with a fuel-bounded `run`,
and proves `fold-sound`/`while-cong`: if the residual one-iteration `body` equals the
source step on every state, the folded `while` computes the **same partial function**
as the source loop (every fuel, every start state) — M2 fold soundness. The contrast
`fold-unsound-witness` is a concrete loop where a wrong body gives a different run, so
the per-iteration "body ≡ source step" obligation is necessary (that per-iteration
obligation is supplied by `spec`/`spec_exp`, differential-tested; full G1 = the
whole-spec congruence).

### `AVExprSound.agda` — expression-level congruence of the specializer (G1 base)

The base case of G1 (whole-spec congruence), complementing `MemoFold` (the loop
level). Models annotated values `av = Sv | Dv | Cv`, residual expression code, the
`lift : av → Expr` readback, and `concretize av env` (the value an av denotes once
the dynamic environment is known). Proves `lift-sound` (`eval (lift av) env ≡
concretize av env` — running the lifted residual reproduces the denoted value) and
`hd-sound`/`tl-sound`/`cons-sound` (the av operations `av_hd`/`av_tl`/`Cv` commute
with concretisation). Together: specializing an expression and running its residual
yields the same value as the source expression — what the command/loop congruence
(G1) is built on.

### `SpecExprStore.agda` — `spec_exp` over a store (G1 middle, expr→command)

Lifts `AVExprSound` from a single annotated value to expressions evaluated against
an **av-store** `σ : ℕ → AV` (what `spec_exp`/`sget` actually do). Defines `sxp`
(`spec_exp` over σ) and a pointwise **consistency** `∀ k → ss k ≡ concretize (σ k)
denv` between the source store `ss` and σ, and proves `spec-exp-sound`: under
consistency, `concretize (sxp e σ) denv ≡ eval e ss` — specialising an expression
against the store and concretising equals evaluating the source expression on the
source store. This is the exact form the command-level congruence consumes (no
function extensionality — consistency is pointwise), connecting `AVExprSound` toward
`MemoFold` (the loop level).

### `SpecCmdStore.agda` — command-level congruence (assignment + sequencing)

The command level of G1, connecting `SpecExprStore` toward `MemoFold`. Adds a
pointwise store `upd` and proves both assignment cases preserve the consistency
invariant: `asgn-fold-sound` (a FOLDED `x := e` updates the av-store, no residual,
rs unchanged — the new slot `spec_exp e σ` concretises to `eval e ss` via
`spec-exp-sound`) and `asgn-emit-sound` (a DYNAMIC `x := e` emits `x :=
lift(spec_exp e σ)`, updating rs at x and setting the slot to the self-reference
`Dv (var x)`). The emit case needs that the *other* slots are unaffected by the
rs-update at x — the no-cross-reference condition of `SpinePreservingEmit`, here as
`SlotDepK k av` (concretising av depends only on rs at index k) and `SelfRef σ`
(every slot depends only on its own index), with `selfref-preserved` showing emit
keeps it. `seq-cong` gives the `'semi` case. The `'if` case is `if-guard-agree`
(the residual guard dispatches like the source, via `lift-sound` + `spec-exp-sound`)
+ `cond-cong` (with guard agreement and both branches reaching the common merged
store, the matching branch preserves consistency; the per-slot merge is `MixMerge`).
The `'while` case (`WhileCase.while-store-cong`) models the loop state as the
(source-store, residual-store) pair under the shared guard (lockstep justified by
`if-guard-agree`) and instantiates `MemoFold.run-inv`: if the body keeps the two
stores consistent, the loop leaves them consistent on exit. So all four command
forms preserve store consistency; only the whole-spec assembly (structural induction
over `spec`) remains for full G1.

### `SpecAssembly.agda` — whole-spec congruence, straight-line fragment (G1 capstone)

The assembly: models `spec : Cmd → av-store → (residual, av-store)` and the source
semantics `cexec` for the straight-line command language (`skip`/`asgn`/`semi`), and
proves `spec-sound` by structural induction — specialising `c` and running its
residual reproduces the source run while preserving store consistency and `SelfRef`.
Each case is discharged by the command-level lemmas: assignment by
`asgn-fold-sound`/`asgn-emit-sound` (with `selfref-fold`/`selfref-preserved` for the
invariant — a folded slot is fully static, so `IsStatic`⇒`SlotDepK`), sequencing by
threading the induction hypothesis. This is `[cogen(p,s)](d) = [p]((s,d))` proven for
the loop-free, conditional-free fragment; extending the assembly to `'if` (per-branch
store merge via `MixMerge`) and `'while` (loop generalisation σloop, then `WhileCase`)
completes G1.

### `RevSpecGarbage.agda` — Theorem A: reversible-PE information lower bound

The proposed theoretical centerpiece (a constraint classical PE does not face).
A *reversible* realization of a specializer `f : X→Y` is an **injective** `g`
with a decode `d`, `d (g x) = f x`. For any pair `f` MERGES (`f x₁ = f x₂`,
`x₁ ≠ x₂`): `separates` (`g x₁ ≠ g x₂`) and `same-residual` (`d(g x₁)=d(g x₂)`),
so the reversible spec is forced to carry **garbage** distinguishing them;
`no-clean-if-merges` / `clean→no-merge` give the dichotomy: a clean reversible
spec is exactly an injective (non-merging) one. Quantitatively, `fibre-map` is
an injection of `f`'s fibre over each residual `o` into `d`'s fibre, so the
garbage at `o` has ≥ as many states as inputs producing `o` (the Bennett–Landauer
count: ≥ log |fibre| bits). Predicts the observed trade-off:

- `mix_triv` (trivial) is **injective** (residual encodes `(p,s)`) ⇒ a clean
  reversible realization exists ⇒ no garbage — why rwhile's specializer is
  trivial-leaning;
- optimizing `mix` is **non-injective** (it folds distinctions away) ⇒ by the
  theorem a reversible version must accumulate garbage — exactly rwhile's
  `spec_av_rev` dead-path garbage embedding.

Complements rwhile's `RWhileRevProjGen.agda` (`proj1-needs-trivial`).

## Relation to the implementation

| proven result | `mix.while` decision |
|---|---|
| `MixBindingTime.fp1-ok` / `fp2-mistags` | `if =? BT 'S` (MKAV) — implemented |
| `MixEmitModel.emit-sound` / `fold-unsound` | `'doasgn` emit + self-ref slots — implemented (unblocked fp2: terminates & correct) |
| `MixPatRead.read-mistags` / `correct-uses` | origin-aware read (PAT-READ-ITER) — optimality: shrink the interpreting `comp2`/`comp3`; not yet implemented |

Still to formalise (future): the dynamic-`if` merge correctness (the `'doif`
dynamic arm).
