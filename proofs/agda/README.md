# mix.while — machine-checked design specs (Agda)

Small, `--safe` Agda models that pin down the design decisions of the partial
evaluator [`examples/desugar/mix.while`](../../examples/desugar/mix.while) and
its road to the Futamura projections. They are *design specs* (à la rwhile's
`proofs/agda/RWhileRevProj2BT.agda`): they prove, in a minimal core, WHY a
particular choice in `mix.while` is the correct one, so the WHILE implementation
can be written/changed against a checked result.

Requires Agda + `standard-library` (this machine: Agda 2.8.0). Typecheck:

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
