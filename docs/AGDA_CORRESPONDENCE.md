# Agda formalization ↔ implementation: what is proven vs tested

Mirrors `rwhile-C-ocaml/AGDA_CORRESPONDENCE.md` for this (classical, non-reversible)
repo. It distinguishes, per implementation component, whether the property is
**machine-checked** (a `--safe` Agda model proves it), **differential-tested only**
(an `examples/desugar/test-*.sh` or OUnit check, no proof), or **unconnected**.
The Agda modules here are *design specs* (small models pinning a decision), except
where noted; the executable artefacts are the OCaml tools and the WHILE programs.

All Agda modules are `--safe` and checked by `proofs/agda/check.sh` (14 modules).

## Correspondence table

| implementation component | Agda result | strength |
|---|---|---|
| `EvalWhile.ml` core semantics (`evalCom`, store) | — | **differential** (`EvalWhileTest.ml` OUnit; `make -C src/core unittest`) |
| `Cogen.ml` BTA **widening** (`widen`, `bt_maxdepth`) | `BTAWiden`: `widen-sound` (`t ⊑ widen d t`), `depth-widen` (`≤ d`) | **proven** (sound over-approximation + finite domain ⇒ fixpoint terminates) |
| `Cogen.ml --online` **spine-preserving emit** (self-ref slot) | `SpinePreservingEmit`: `readback` (faithful), `stable` (sound under re-materialisation), `cross-unsound` | **proven** (the C5 enabler; spine case of the emit model) |
| `Cogen.ml --online` emit-vs-fold under dynamic control (`dynamicize`, self-ref slot) | `MixEmitModel`: `emit-sound`, `fold-unsound`, `fold-tracks-new` | **proven (design spec)**; the OCaml emit path is the same principle, differential-tested |
| `Cogen.ml --online` value-driven control (unroll on `Sv`/`Cv` guard) | — | **differential** (`test-poly`, `test-jones`, `test-usi`, `test-bench`) |
| `Cogen.ml` **M2 memo fold** (`drive`/`drive_dyn_if`, `COGEN_MEMO`) | `MemoFold`: `fold-sound`/`while-cong` (folded `while` ≡ source loop when body preserves the step), `fold-unsound-witness` | **proven (kernel)**; the per-iteration "body ≡ source step" obligation is differential (the loop-level fold is the proven part) |
| `Cogen.ml` **expression specializer** (`spec_exp`, `lift`, `av_hd`/`av_tl`) | `AVExprSound`: `lift-sound` (`eval (lift av) ≡ concretize av`), `hd-sound`/`tl-sound`/`cons-sound` (av ops commute with concretisation) | **proven** (the expression-level congruence — the base case of G1) |
| `Cogen.ml` **`spec_exp` over a store** (`sget`-indexed eval) | `SpecExprStore`: `spec-exp-sound` (consistent store ⇒ `concretize (spec_exp e σ) ≡ eval e ss`) | **proven** (the expression-over-store congruence; the form the command level uses) |
| `Cogen.ml` **command spec** (`spec` `'asgn` fold+emit, `'semi`, `'if`, `'while`) | `SpecCmdStore`: `asgn-fold-sound`, `asgn-emit-sound`, `seq-cong`, `SlotDepK`/`SelfRef`/`selfref-preserved`, `if-guard-agree`+`cond-cong`, `WhileCase.while-store-cong` (loop preserves consistency, via `MemoFold.run-inv`), `dyn1-sound`/`dynlist-sound` (the generalisation step — dynamicise a var / a list of vars with residual fixups — shared by the `'if` merge and `'while` loop-entry) | **proven** (all four command forms preserve store consistency) |
| `Cogen.ml` **whole-spec congruence** (straight-line) | `SpecAssembly`: `spec-sound` — by structural induction `[spec c](residual) reproduces [c](source)`, preserving consistency, for `skip`/`asgn`/`semi`; `frame` (spec preserves the store off assigned vars) | **proven** (assembled `[cogen(p,s)](d)=[p]((s,d))` for the loop-free, if-free fragment) |
| `Cogen.ml` **whole-spec congruence — `'if`** | `SpecAssemblyIf`: `spec-if-sound` — the conditional's result is consistent with the merged store `dynList esc σ`, via `frame`+`dynList-cong` (branches agree off the escaping set) + `dynlist-sound` (fixups) + `if-guard-agree`+`cond-cong` | **proven** (the `'if` case assembled) |
| `Cogen.ml` **whole-spec congruence — `'while`** | `SpecAssemblyWhile`: `spec-while-entry` (loop entry: generalising the assigned set makes the entry stores consistent with σloop — corollary of `dynlist-sound`) + `spec-while-sound` (given σloop is a fixpoint of the body specialiser, the loop preserves consistency — `spec-sound` rewritten along the fixpoint, fed to `WhileCase.while-store-cong` = `MemoFold.run-inv`); `example-while-sound` discharges the fixpoint hypothesis by computation on a concrete loop (non-vacuity) | **proven** (the `'while` case assembled — ties body congruence to the loop kernel; G1 now covers all command forms) |
| `Cogen.ml --online` **online-termination** (bounded unroll + generalization) | widening half: `BTAWiden`; the unroll-budget/generalization half: — | **partial**: widening **proven**; generalization **differential** (`test-terminate`) |
| `Cogen.ml` offline cogen / `--gen` generating extension | — | **differential** (`test-cogen`, `test-genext`) |
| `Cogen.ml` / `rundata` ⌜p⌝ oracle agreement | — | **differential** (`test-oracle`) |
| `mix.while` MKAV binding-time test (`if =? BT 'S`) | `MixBindingTime`: `fp1-ok`, `fp2-mistags`, `correct-uses-input`, `buggy-ignores-input` | **proven (design spec)** |
| `mix.while` `'doasgn` emit model (self-referential slots) | `MixEmitModel`: `emit-sound` / `fold-unsound` | **proven (design spec)** |
| `mix.while` PAT-READ (binding-time-aware read) | `MixPatRead`: `read-mistags`, `correct-uses`, `read-ignores-origin` | **proven (design spec)** |
| `mix.while` dynamic-`if` store merge (`'doif`) | `MixMerge`: `merge-correct`, `merge-needs-both`, `merge-buggy-wrong` | **proven (design spec)** |
| reversible-PE information bound (Theorem A) | `RevSpecGarbage`: `no-clean-if-merges`, `direct-needs-injective`, `clean-reversible`, `Fibre` | **proven (abstract)**; *measured* in `rwhile-C-ocaml` (`FINDINGS §6`) |

## Gaps (in priority order)

- **G1 — spec correctness end-to-end (ASSEMBLED for the full language).** Built
  **bottom-up** and now assembled for **every command form**: `AVExprSound`
  (expression level) → `SpecExprStore` (expressions over a store) → `SpecCmdStore`
  (all command forms preserve the store-consistency invariant: `'asgn` fold/emit,
  `'semi`, `'if`, `'while`) → `MemoFold` (loop level) → **`SpecAssembly.spec-sound`**
  (`skip`/`asgn`/`semi`) → **`SpecAssemblyIf.spec-if-sound`** (the `'if` case:
  branches merged at the escaping set via `frame`+`dynList-cong`+`dynlist-sound`,
  dispatched by `if-guard-agree`+`cond-cong`) → **`SpecAssemblyWhile.spec-while-sound`**
  (the `'while` case: entry by generalising the assigned set — `spec-while-entry` /
  `dynlist-sound`; loop by `spec-sound` rewritten along the σloop fixpoint, fed to
  `WhileCase.while-store-cong` = `MemoFold.run-inv`). So `[cogen(p,s)] (d) =
  [p]((s,d))` is machine-checked for the **whole language**
  (`skip`/`asgn`/`semi`/`if`/`while`). The `'while` case carries one explicit
  hypothesis — σloop is a **generalisation fixpoint** of the body specialiser — which
  is exactly the loop-store well-formedness Cogen.ml's iterate-to-fixpoint
  establishes (so the residual loop folds) — and `example-while-sound` shows it is
  **non-vacuous**, discharging it by computation on a concrete decrementing loop
  (`while x₀ do x₀ := tl x₀`, all-dynamic σ⋆), so the `'while` soundness fires with
  nothing assumed. The earlier pieces (`MixEmitModel`,
  `SpinePreservingEmit`, `BTAWiden`, `MixMerge`) pin the individual decisions;
  whole-spec correctness for the full language is also differential-tested
  (`run-all.sh`). (cf. rwhile's `RWhileAVSound`/`RWhileAVSpec`.)
- **G2 — quasi-termination (whole specializer total).** `BTAWiden` gives BTA
  termination; the online unroll budget + generalization (`test-terminate`) is only
  differential. Modeling the generalization fallback would make "total" proven.
  The principled, citable route is a homeomorphic-embedding whistle (Leuschel 2002;
  TbHEm for the C-domain) — see `docs/RELATED-WORK.md` §C5(i).
- **G3 — generating extension (`--gen`) correctness.** Differential only.

## Relation to `rwhile-C-ocaml`

The reversible sibling tracks the same proven-vs-tested distinction
(`rwhile-C-ocaml/AGDA_CORRESPONDENCE.md`, 36 `--safe` modules). Two concrete ties:

- `RevSpecGarbage` (here, Theorem A lower bound) ↔ `RWhileRevProjGen` /
  `RWhileGarbageBound` (there, with an *achievable upper* bound) — together bracket
  the reversible specializer's garbage `|fibre| ≤ garbage ≤ |X|` (see
  `docs/PAPER-SKELETON.md` §(c)).
- The **store-index binding-time wall** (this repo's `usi`/store-walk;
  `docs/FUTAMURA.md`) is the same one rwhile diagnoses in `analysis_store_bti.md`
  (their `comp2`'s 125 residual store loops). Our `usi` + spine-preserving emit
  *realize their open candidate (A)* (partially-static store) for the classical
  case — see that file and `rwhile-C-ocaml/analysis_store_bti_classical_ref.md`.
