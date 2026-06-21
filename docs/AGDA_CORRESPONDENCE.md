# Agda formalization ↔ implementation: what is proven vs tested

Mirrors `rwhile-C-ocaml/AGDA_CORRESPONDENCE.md` for this (classical, non-reversible)
repo. It distinguishes, per implementation component, whether the property is
**machine-checked** (a `--safe` Agda model proves it), **differential-tested only**
(an `examples/desugar/test-*.sh` or OUnit check, no proof), or **unconnected**.
The Agda modules here are *design specs* (small models pinning a decision), except
where noted; the executable artefacts are the OCaml tools and the WHILE programs.

All Agda modules are `--safe` and checked by `proofs/agda/check.sh` (8 modules).

## Correspondence table

| implementation component | Agda result | strength |
|---|---|---|
| `EvalWhile.ml` core semantics (`evalCom`, store) | — | **differential** (`EvalWhileTest.ml` OUnit; `make -C src/core unittest`) |
| `Cogen.ml` BTA **widening** (`widen`, `bt_maxdepth`) | `BTAWiden`: `widen-sound` (`t ⊑ widen d t`), `depth-widen` (`≤ d`) | **proven** (sound over-approximation + finite domain ⇒ fixpoint terminates) |
| `Cogen.ml --online` **spine-preserving emit** (self-ref slot) | `SpinePreservingEmit`: `readback` (faithful), `stable` (sound under re-materialisation), `cross-unsound` | **proven** (the C5 enabler; spine case of the emit model) |
| `Cogen.ml --online` emit-vs-fold under dynamic control (`dynamicize`, self-ref slot) | `MixEmitModel`: `emit-sound`, `fold-unsound`, `fold-tracks-new` | **proven (design spec)**; the OCaml emit path is the same principle, differential-tested |
| `Cogen.ml --online` value-driven control (unroll on `Sv`/`Cv` guard) | — | **differential** (`test-poly`, `test-jones`, `test-usi`, `test-bench`) |
| `Cogen.ml` **M2 memo fold** (`drive`/`drive_dyn_if`, `COGEN_MEMO`) | `MemoFold`: `fold-sound`/`while-cong` (folded `while` ≡ source loop when body preserves the step), `fold-unsound-witness` | **proven (kernel)**; the per-iteration "body ≡ source step" obligation is differential (the loop-level fold is the proven part) |
| `Cogen.ml --online` **online-termination** (bounded unroll + generalization) | widening half: `BTAWiden`; the unroll-budget/generalization half: — | **partial**: widening **proven**; generalization **differential** (`test-terminate`) |
| `Cogen.ml` offline cogen / `--gen` generating extension | — | **differential** (`test-cogen`, `test-genext`) |
| `Cogen.ml` / `rundata` ⌜p⌝ oracle agreement | — | **differential** (`test-oracle`) |
| `mix.while` MKAV binding-time test (`if =? BT 'S`) | `MixBindingTime`: `fp1-ok`, `fp2-mistags`, `correct-uses-input`, `buggy-ignores-input` | **proven (design spec)** |
| `mix.while` `'doasgn` emit model (self-referential slots) | `MixEmitModel`: `emit-sound` / `fold-unsound` | **proven (design spec)** |
| `mix.while` PAT-READ (binding-time-aware read) | `MixPatRead`: `read-mistags`, `correct-uses`, `read-ignores-origin` | **proven (design spec)** |
| `mix.while` dynamic-`if` store merge (`'doif`) | `MixMerge`: `merge-correct`, `merge-needs-both`, `merge-buggy-wrong` | **proven (design spec)** |
| reversible-PE information bound (Theorem A) | `RevSpecGarbage`: `no-clean-if-merges`, `direct-needs-injective`, `clean-reversible`, `Fibre` | **proven (abstract)**; *measured* in `rwhile-C-ocaml` (`FINDINGS §6`) |

## Gaps (in priority order)

- **G1 — spec correctness end-to-end.** No module yet proves
  `[cogen --online(p,s)](d) = [p]((s,d))` for the *actual* `Cogen.ml` algorithm;
  the pieces (`MixEmitModel`, `SpinePreservingEmit`, `BTAWiden`, `MixMerge`) pin the
  individual decisions, and correctness is differential-tested (`run-all.sh`).
  Closing G1 = an Agda model of the AV specializer's `spec`/`spec_exp` with a
  congruence proof (cf. rwhile's `RWhileAVSound`/`RWhileAVSpec`, which did this for
  their reversible AV specializer — a template).
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
