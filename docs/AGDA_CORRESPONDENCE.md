# Agda formalization ↔ implementation: what is proven vs tested

Mirrors `rwhile-C-ocaml/AGDA_CORRESPONDENCE.md` for this (classical, non-reversible)
repo. It distinguishes, per implementation component, whether the property is
**machine-checked** (a `--safe` Agda model proves it), **differential-tested only**
(an `examples/desugar/test-*.sh` or OUnit check, no proof), or **unconnected**.
The Agda modules here are *design specs* (small models pinning a decision), except
where noted; the executable artefacts are the OCaml tools and the WHILE programs.

All Agda modules are `--safe` and checked by `proofs/agda/check.sh` (23 modules).

## Correspondence table

| implementation component | Agda result | strength |
|---|---|---|
| `EvalWhile.ml` core semantics (`evalCom`, store) | — | **differential** (`EvalWhileTest.ml` OUnit; `make -C src/core unittest`) |
| `Cogen.ml` BTA **widening** (`widen`, `bt_maxdepth`) | `BTAWiden`: `widen-sound` (`t ⊑ widen d t`), `depth-widen` (`≤ d`) | **proven** (sound over-approximation + finite domain ⇒ fixpoint terminates) |
| `Cogen.ml --online` **spine-preserving emit** (self-ref slot) | `SpinePreservingEmit`: `readback` (faithful), `stable` (sound under re-materialisation), `cross-unsound` | **proven** (the C5 enabler; spine case of the emit model) |
| `Cogen.ml --online` emit-vs-fold under dynamic control (`dynamicize`, self-ref slot) | `MixEmitModel`: `emit-sound`, `fold-unsound`, `fold-tracks-new` | **proven (design spec)**; the OCaml emit path is the same principle, differential-tested |
| `Cogen.ml --online` value-driven control (unroll on `Sv`/`Cv` guard) | — | **differential** (`test-poly`, `test-jones`, `test-usi`, `test-bench`) |
| `Cogen.ml` **M2 memo fold** (`drive`/`drive_dyn_if`, `COGEN_MEMO`) | `MemoFold`: `fold-sound`/`while-cong` (folded `while` ≡ source loop when body preserves the step), `fold-unsound-witness` | **proven (kernel)**; the per-iteration "body ≡ source step" obligation is differential (the loop-level fold is the proven part) |
| `Cogen.ml` **M3b structured reconstruction** (`m3_reconstruct_general`, `COGEN_M3`) | `MemoReconstruct`: `Recon.sound` (the single-natural-loop flowchart `H:sH;if g then B else X · B:sB;goto H · X:sX;halt` and the emitted `sH; while g {sB;sH}; sX` compute the same partial function) | **proven (kernel)**; M4 — the loop-RECONSTRUCT soundness, companion to `MemoFold`'s loop-FOLD soundness for the assembled M3 driver |
| `Cogen.ml` **M3b reconstruction — general/nested** (`m3_reconstruct_general`) | `MemoReconstructGen`: `Recon.loop-sound` (the header flowchart loop ≡ the emitted `sH; while g {body; sH}; exit` for a **partial body AND partial exit** `S → Maybe S`, so loops nest in bodies AND compose with arbitrary — incl. looping — continuations) + `branch-sound`/`seq-sound` (the if-branch and forward-edge rules) | **proven (M4)**; generalises the single-loop kernel to nested/partial bodies+exits — every reconstruction rule is semantics-preserving |
| `Cogen.ml` **M3c whistle termination** (`m3_specialize` generalization) | `WhistleTerminates`: `gen-strict` (a generalization strictly increases the dynamic-slot count) + `trues-bound` (≤ slot count) ⇒ `gen-bound` (finitely many generalizations) | **proven (M4)**; the implementation's monotone-bounded termination argument (a dynamicised slot is terminal), not a full Higman/Kruskal wqo |
| `Cogen.ml` **M3 global-memo loop** (`m3_seen`, `drive2`) | `MemoDriverSound`: `Driver.drive-transparent` (a consistent memo yields exactly `spec c` on hit or miss) + `store-preserves` (recording `(c, spec c)` keeps consistency) | **proven (M4)**; memoization is TRANSPARENT — sharing identical configs ≡ non-memoized per-config specialization; per-config soundness is G1, fold/reconstruct are `MemoFold`/`MemoReconstructGen` |
| `Cogen.ml` **expression specializer** (`spec_exp`, `lift`, `av_hd`/`av_tl`) | `AVExprSound`: `lift-sound` (`eval (lift av) ≡ concretize av`), `hd-sound`/`tl-sound`/`cons-sound` (av ops commute with concretisation) | **proven** (the expression-level congruence — the base case of G1) |
| `Cogen.ml` **`spec_exp` over a store** (`sget`-indexed eval) | `SpecExprStore`: `spec-exp-sound` (consistent store ⇒ `concretize (spec_exp e σ) ≡ eval e ss`) | **proven** (the expression-over-store congruence; the form the command level uses) |
| `Cogen.ml` **command spec** (`spec` `'asgn` fold+emit, `'semi`, `'if`, `'while`) | `SpecCmdStore`: `asgn-fold-sound`, `asgn-emit-sound`, `seq-cong`, `SlotDepK`/`SelfRef`/`selfref-preserved`, `if-guard-agree`+`cond-cong`, `WhileCase.while-store-cong` (loop preserves consistency, via `MemoFold.run-inv`), `dyn1-sound`/`dynlist-sound` (the generalisation step — dynamicise a var / a list of vars with residual fixups — shared by the `'if` merge and `'while` loop-entry) | **proven** (all four command forms preserve store consistency) |
| `Cogen.ml` **whole-spec congruence** (straight-line) | `SpecAssembly`: `spec-sound` — by structural induction `[spec c](residual) reproduces [c](source)`, preserving consistency, for `skip`/`asgn`/`semi`; `frame` (spec preserves the store off assigned vars) | **proven** (assembled `[cogen(p,s)](d)=[p]((s,d))` for the loop-free, if-free fragment) |
| `Cogen.ml` **whole-spec congruence — `'if`** | `SpecAssemblyIf`: `spec-if-sound` — the conditional's result is consistent with the merged store `dynList esc σ`, via `frame`+`dynList-cong` (branches agree off the escaping set) + `dynlist-sound` (fixups) + `if-guard-agree`+`cond-cong` | **proven** (the `'if` case assembled) |
| `Cogen.ml` **whole-spec congruence — `'while`** | `SpecAssemblyWhile`: `spec-while-entry` (loop entry: generalising the assigned set makes the entry stores consistent with σloop — corollary of `dynlist-sound`) + `spec-while-sound` (given σloop is a fixpoint of the body specialiser, the loop preserves consistency — `spec-sound` rewritten along the fixpoint, fed to `WhileCase.while-store-cong` = `MemoFold.run-inv`); `example-while-sound` discharges the fixpoint hypothesis by computation on a concrete loop (non-vacuity) | **proven** (the `'while` case assembled — ties body congruence to the loop kernel; G1 now covers all command forms) |
| **Futamura projections as a meta-theorem** (the three projections of `mix`/`Cogen.ml`) | `FutamuraProjections`: from a single `MixModel.mix-eq` (`[ [mix](p,s) ](d) ≡ [p](s,d)`) derive `fp1`/`fp2`/`fp3` (each an instance of `mix-eq` at args `(int,source)`/`(mix,int)`/`(mix,mix)`) + `fp2-correct`/`fp3-correct` (the composed chain: cogen ▸ compiler ▸ target ≡ int); `Witness.model` is a faithful non-degenerate self-applicable specializer discharging `mix-eq` by `refl` | **proven (meta)**; reduces all three projections to the spec equation — whose operational content is G1 (`SpecAssembly*`), the hypothesis this module consumes |
| **Futamura meta-theorem for a PARTIAL `run`** (the Turing-complete setting) | `FutamuraPartial`: `PartialMixModel` has `run : Data → Data → Maybe Data`, `mix-runs` (mix terminates → residual) + `mix-eq` (residual correct as Maybe-values); derives `fp1`/`fp2`/`fp3` in Maybe form (compiler/cogen *run* — `≡ just …` — to the next stage) + `fp2-correct`/`fp3-correct` (whatever they return is a correct target/compiler, via just-injectivity); `Witness.model` is genuinely partial (data-as-program ⇒ `nothing`) | **proven (meta)**; the `Maybe`-relative generalisation of `FutamuraProjections`, the form `UniversalInterp`'s partial `run` instantiates once `mix` is an `Exp` |
| **Programs-as-data** for the universal interpreter (⌜p⌝ encoding) | `ProgEncoding`: a tagged encoding `⌜_⌝ : Exp → V` (tags = unary numerals; a code is `(tag . args)`) and a partial `decode : V → Maybe Exp`, with `decode-⌜⌝` (decode is a left inverse: `decode ⌜e⌝ ≡ just e`) + concrete round-trips | **proven (machinery)**; the in-Agda counterpart of `DesugarWhile --data`'s ⌜p⌝ — the form a self-interpreter / `mix` written in `Exp` consumes |
| **Universal interpreter** (the partial `run ⌜p⌝ d` for a Turing-complete object language) | `UniversalInterp`: a fuel-indexed evaluator `eval : ℕ → Exp → V → Maybe V` for a minimal Turing-complete first-order recursive S-expression language (total in Agda, partial as a semantics); `mono`/`add-fuel` (more fuel never changes a defined result); concrete `refl` runs of a genuine `reverse` program (it computes `[a,b,c] ↦ [c,b,a]` inside Agda, and is `nothing` under-fuelled) | **proven (machinery)**; supplies the partial `run` G4 needs — the in-Agda counterpart of `universal.while` / `DesugarWhile --data` |
| **Concrete s-m-n witness** (a real specializer instantiating the meta-theorem) | `FutamuraSmn`: a one-input straight-line term language (`Tm`, `ev1`) + a genuine specializer `spec t sv = sub t (kcons (lit sv) inp)` with `sub-correct` (substitution lemma) ⇒ `spec-correct` (`[spec t sv](d) ≡ [t](sv,d)` — static input *constant-folded*, not deferred); `model : MixModel` discharges `mix-eq` BY `spec-correct` for first-order programs (closure law for the mix-on-mix case), with concrete `refl` checks that fp1's target runs on the dynamic input ALONE and recovers the folded static value | **proven (concrete)**; the finite, in-Agda analogue of G1 with a genuine binding-time split. *The wall:* full self-application (mix folding mix itself) needs the object language to express its own specializer (universality/recursion) — scoped as the universal-WHILE-interpreter milestone (G4 below) |
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
  (`run-all.sh`). (cf. rwhile's `RWhileAVSound`/`RWhileAVSpec`.) **Consumed by the
  meta-theorem `FutamuraProjections`:** G1 *is* the operational content of the
  abstract `mix-eq`, from which `fp1`/`fp2`/`fp3` and their composed correctness
  are derived as instances — so "all three projections are correct" is reduced,
  machine-checked, to this one congruence.
- **G2 — quasi-termination (whole specializer total).** Largely closed. `BTAWiden`
  gives BTA termination; the global-memo driver's **configuration**-set finiteness is
  the M3c homeomorphic-embedding whistle (Leuschel 2002), whose generalization
  termination is now **machine-checked** (`WhistleTerminates`: monotone-bounded — each
  slot dynamicised at most once). Note the wqo route (Higman/Kruskal) is *inapplicable*
  here — the embedding tests equality over unbounded static values, not a wqo — so the
  monotone-bounded argument is the right one (see `docs/RELATED-WORK.md` §C5(i)).
  *Remaining differential:* the straight-line `--online` unroll budget + generalization
  fallback (`test-terminate`); modeling that would make "total" fully proven.
- **G3 — generating extension (`--gen`) correctness.** Differential only.
- **G4 — genuine specialization through self-application (the s-m-n wall).**
  *In progress.* `FutamuraSmn` instantiates the meta-theorem with a real
  specializer (`spec`, `spec-correct`: the static input is *constant-folded*), so
  the **first** projection's specialization is concrete and machine-checked; but
  its straight-line object language cannot express its own specializer, so
  fp2/fp3's mix-on-mix self-application routes through the closure law (sound, not
  folding). **Step 1 done — `UniversalInterp`** builds the missing machinery: a
  fuel-indexed universal interpreter (`eval`/`run`) for a Turing-complete
  first-order recursive S-expression language, with fuel-monotonicity (`mono`,
  `add-fuel`) and concrete `refl` runs of a real `reverse` program — the in-Agda
  counterpart of `universal.while` / `DesugarWhile --data`, and the partial `run`
  a self-applicable `mix` requires. **Step 2 done — `FutamuraPartial`** supplies the
  `Maybe`-relative meta-theorem (`PartialMixModel`: `run : Data → Data → Maybe
  Data`, `mix-runs` + `mix-eq`; fp1/fp2/fp3 + composed correctness derived), the
  exact interface `UniversalInterp`'s partial `run` instantiates. **Remaining
  (the last piece)** — write the specializer `mix` AS an `Exp` program (now
  expressible: the language has the recursion to walk ⌜p⌝) and prove `run ⌜mix⌝
  (⌜p⌝ , s) ≡ just ⌜spec p s⌝`, discharging `PartialMixModel` concretely; then
  fp2/fp3 fold genuinely through self-application. The abstract `mix-eq`, the
  concrete first-order discharge (`FutamuraSmn`), the partial interface
  (`FutamuraPartial`), and the universal `run` (`UniversalInterp`) now bracket
  exactly that one remaining construction.

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
