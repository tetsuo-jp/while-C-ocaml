# Levin optimal search — the strengthened contribution story (for a PRO paper)

This doc collects the arguments that lift the classical Levin formalization above
"we mechanized a known theorem". It is the contribution/novelty writeup for a paper
on the (classical + reversible) Levin optimal-search mechanization — distinct from
`PAPER-SKELETON.md` (the partial-evaluation / Jones-optimality paper). Novelty
evidence and related work: `RELATED-WORK.md` (Levin section, with the 2026-07
literature check).

## 0. Novelty (settled by literature check, 2026-07)

First machine-checked proof of Levin's optimal search theorem (Jones,
*Computability and Complexity*, Thm 20.1.2). Confirmed absent from Coq
(Forster's synthetic-computability / undecidability libs), HOL4 (Catt–Norrish
KC), Isabelle AFP (only `Cook_Levin` = SAT NP-completeness, **a different Levin
theorem** — must be distinguished), and Lean/mathlib (computability foundations,
no KC / no Levin search). Nearest neighbours are two **Kolmogorov-complexity**
formalizations (Forster–Kunze–Lauermann, ITP 2022; Catt–Norrish, CPP 2021), whose
own argument we mirror (see §below and `RELATED-WORK.md`).

Because a mechanization of a *known* theorem must offer more than the theorem, the
six strengthening lines below are the actual paper contributions.

## ① The mechanization vs Jones's informal proof (what precision it forced)

**Honest bottom line: the mechanization found NO flaw or hole in Jones's argument.**
Thm 20.1.2 is faithful — every step decomposes into the arithmetic core +
interpreter-overhead lemma + enumeration, as Jones sketches. "We confirm the
textbook proof is sound" is itself the honest headline; we do **not** manufacture a
gap. What the mechanization *does* add is (a) a concrete, twice-checked constant with
the modelling commitments it forces, (b) a methodological negative result, and
(c) the exact shape of the optimality constant — plus the honest limitations that
keep the theorem statement precise.

Findings (audited against the code, ranked; tag = kind of contribution):

1. **[CONSTANT] Jones's existential overhead `a` → concrete `a = 73`, twice checked.**
   `IntEfficient.agda:39-40`; `73 = max` of the seven per-node dispatch costs
   `{var 8, knil 12, khd 39, ktl 48, kcons 45, kifnil 73, call 64}` (`SintCost.agda`),
   reproduced at the *same* value in Rocq (`SintCostR.v`/`IntEfficientR.v`). Making it
   a theorem forced three commitments: (i) cost model = one unit-cost step per `Exp`
   node summed over sub-evaluations (`StepCount.evalT`, `CostRelation`), matching the
   book's `time_p` / `while --time`; (ii) the specific interpreter = the lean
   one-variable I self-interpreter `sint : Exp` (`SelfInterp.agda`); (iii) uniform
   per-node charges (`bound1`/`bound2` need every `K ≤ a`). **This is the single
   strongest item.**
2. **[PRECISION / technique] `refl`-dispatch mechanizes self-interpreter *correctness*
   but NOT *cost*.** Correctness (`SelfInterpCorrect.simF`) closes each node by a
   `refl` dispatch lemma; the step count cannot (the sub-run's count is trapped inside
   a stuck symbolic self-interpretation). The fix — a fuel-free **big-step cost
   relation** with opaque sub-derivations (`CostRelation._,_⇓_#_`) + derivation-tree
   dispatch-cost lemmas (`SintCost`) — is why `IntEfficient` is shaped as it is.
   Reportable methodological result: *verifying interpreter efficiency is not a
   step-accounting refinement of verifying correctness.* Pairs with #1.
3. **[PRECISION] the optimality constant is exactly `a_q = 4·a·2^j`, and the `4` is
   decomposed** (`LevinOptimal.agda:67-74`): `4 = 2×2`, one `2` from the geometric-sum
   bound `Σ_{i≤k}2^i < 2·2^k` (`LevinBudget.gsum<2last`), one from the overshoot
   `least 2^k ≥ N ≤ 2N` (`LevinBudget.overshoot`); `2^j` from the doubling schedule.
4. **[PRECISION] `2^j` is `2^(Cantor Gödel index)`, not `2^|q|`** (`NatPairing.pair`,
   `LevinDovetailRun.hit`). Jones's "depends on `q`'s size" implicitly assumes a
   size-based numbering; the mechanization exposes that schedule+numbering (not size)
   drive the constant's magnitude (still input-independent).
5. **[PRECISION] the batch cost model** makes `time-opt k = gsum k` definitional
   (`LevinDovetail.agda:50`); the factor-2 gap to the literal dovetail step count
   `Σ_{m≤i}pow2(i∸m) ≈ 2·pow2 i` is folded into `a_q` (standard for Levin bounds),
   documented not proved away.
6. **[PRECISION] the operational finish `k_op ≤ k` is made explicit**
   (`LevinComplete.levin-operational` + `gsum-mono`): a faster program may verify
   first; monotonicity preserves the bound.
7. **[ROUTINE] discharged obligations, no Jones-level content:** positivity `1 ≤ c·S`,
   Cantor pairing inverse (structurally recursive, no WF machinery), surjective
   decoder, geometric-sum/overshoot arithmetic, `evalT`↔`⇓` adequacy.

### Must-disclose honesty caveats (state these, or a reviewer will)

These are limitations of the *formalization* (not of Jones), and stating them makes
the contribution precise rather than weaker:

- **`a = 73` is the one-variable-I `sint` constant under a per-node unit-cost model.**
  It is NOT the overhead of an actual multi-variable WHILE universal program:
  `docs/EFFICIENCY.md` measures the timed-universal `tt` at ratio `≈ 2350`, growing
  `O(#vars)`; the WHILE→I translation cost is not mechanized. (An order-of-magnitude
  mismatch a reviewer will spot — own it.)
- **The verifier `r` is not step-charged.** The checker is a free semantic oracle
  `chk : V → V → Bool` (`LevinDovetailRun.agda`); the proved bound is `gsum k < a_q·T_q`,
  weakened to `a_q·(T_q+T_r)` by padding `T_q ≤ T_q+T_r`. `T_r` is decorative slack;
  charging `r` through `sint` is future work.
- **`opt` is an Agda meta-driver, not a literal `Exp` program.** `find`/`done`/`runFor`
  are Agda functions; the literal `opt ∈ Exp` is only partially built (`LevinOptE`,
  `LevinBudgetE`). The theorem is about a dovetailing driver under the batch cost
  model, not a step-counted WHILE program.

**Honest theorem statement for the paper:** *Levin optimality for a dovetailing
driver, with linear interpreter overhead `a = 73` proven (twice) for the one-variable
I self-interpreter under a per-node unit-cost model, the verifier modelled as a free
oracle.*

## ② A reusable, cost-model-agnostic framework (not a one-off) — PROVEN reusable

The optimality argument is factored as **Layer A** (`LevinOptimal.agda`): the
budget-doubling dovetail optimality inequality proved as an *abstract meta-theorem*,
parametric in the interpreter overhead — it depends only on a few `ℕ`-valued record
fields (`aq`, `c`, `S`), NOT on any concrete interpreter. The self-interpreter and
its cost (`UniversalInterp`/`SelfInterp`/`SintCost`/`IntEfficient`) discharge those
fields separately (`LevinGrounded`).

**The reuse is not a claim — it is checked.** The *novel* reversible-Levin result
instantiates the SAME Layer A verbatim: `RevLevinAbstract`, `RevLevinGrounded`,
`RevLevin` all `open import LevinOptimal using (LevinSearch; levin-optimal; aq; c;
S)` and `LevinBudget using (gsum)` — zero change to Layer A. So the classical
formalization is demonstrably a **reusable framework**: the entire dovetail
superstructure (`LevinBudget`, `NatPairing`, `LevinOptimal`, `LevinDovetail{,Run}`,
`LevinComplete` — the cost-model-agnostic layers) fires again, unmodified, for a
theorem Jones never stated. This is the antidote to "rote formalization": the
artifact's value is that it *composes into new results*.

## ③ The interpretive-overhead constant as a first-class theorem (+ empirical tie)

Jones asserts an overhead constant `a` exists (Thm 19.1.3) but never computes it;
the optimality bound `a_q·(T_q+T_r)` rides on that unquantified `a`. We make it a
**theorem**: `IntEfficient` proves `a = 73` (per-node dispatch charges summed over
the self-interpreter, `SintCost`), and `LevinGrounded` feeds it back so Levin
optimality holds with **no overhead hypothesis**. This is the anti-hand-waving move
— the analogue, in our setting, of Forster et al. eliminating the one classical step
from the KC proof.

The constant is not left as a bare number: the reversible twin `a-rev` and the
measured `≈364×` (`rwhile-C-ocaml/measure_ri_overhead.sh`) are reconciled into **one
affine law** `overhead ≤ (base + 2·m·L)·ocost` (`RevCostReconcile.agda`), with the
node-summed cost fixed as canonical. So "interpretive overhead" is not a fudge
factor but a machine-checked, empirically-anchored quantity.

**Scope (see ① caveat):** `a = 73` is the overhead of the *one-variable I*
self-interpreter `sint` under a per-node unit-cost model — not of a multi-variable
WHILE universal program (which `docs/EFFICIENCY.md` measures at `≈2350`, `O(#vars)`).
State this explicitly; the contribution is "the abstract constant is now a concrete,
twice-checked theorem for a specified interpreter + cost model", not "73 is the
universal overhead".

## ④ Extraction + differential testing = validated faithfulness

The proof is not just symbols. The machine-checked Rocq interpreter is **extracted to
OCaml** (`proofs/rocq/Extract.v` → `while_eval.ml{,i}`, committed): the verified `run`
(= `UniversalInterp.eval`), the self-interpreter `sint`, and the `enc`/`decode` ⌜p⌝
codec. `Extract.v` is a member of `proofs/rocq/check.sh`, so it compiles with the
proof build.

- **`test-extract.sh`** runs the *extracted verified* interpreter as OCaml and checks
  it against the proved oracle values on: every `Exp` constructor; `sint` == the
  `SelfInterp` oracle; **G1 at runtime** (for a program, direct `run p` = its
  self-interpretation `selfrun p` — `simF_full` exhibited executably); larger
  **recursive** list programs (`len`/`append`/`reverse`); and `decode ∘ enc = id`.
  (Skips cleanly if `rocq`/`ocamlc` absent; recursion under self-interpretation is a
  documented omission — the extracted `eval` is strict.)
- **`test-oracle.sh`** cross-checks the ⌜p⌝-interpreter `rundata` against the
  reference `EvalWhile`/`while` with **three-way agreement across both ⌜p⌝ encodings**
  (`--data` = if→while expanded, `--data-if` = first-class if), over
  `reverse-case`/`int_min`/`int_loop`/`int_if` × boundary inputs — removing the
  "wrong-oracle" blind spot.
- **`test-cogen.sh`** extends the differential test to the specializer with
  **deterministic fuzz** (seeded nested-atom-list inputs, seeds 1–6 over
  int_min/int_loop/int_if/int_nest).
- All wired into one `run-all.sh` (~24 scripts) + `proofs/agda/check.sh`.

**Paper line:** the extracted verified interpreter *runs and agrees with the
reference* on every constructor, on recursive list programs, on the self-interpreter,
and on G1 at runtime — with a second differential layer (`rundata` vs `EvalWhile`,
both encodings, incl. fuzz). Faithfulness of the formalization is validated
executably, not merely asserted.

## ⑤ Double verification (Agda + Rocq): a mechanization-methodology study

The Levin chain is proved **twice** — Agda `--safe` (83 modules) and Rocq 9.1 (the
22-module twin), both axiom-free (`Print Assumptions` = closed). This rules out a
single-prover artifact **and** yields a reportable "lessons learned": redoing the
same theorem isolates one genuine system-level divergence and several reassuring
convergences.

| Aspect | Agda (`--safe`) | Rocq 9.1 | Evidence |
|---|---|---|---|
| self-interp **per-node dispatch** | by **`refl`** (definitional) | needs **`vm_compute`**; kernel `reflexivity` **exponential** on multi-sub (kcons ~100 s, kifnil >300 s) | `SelfInterpCorrect.agda` vs `SimF.v` |
| dispatch-equation RHS | clean form fine | must be **shape-faithful** (nested matches) — `vm_compute` won't collapse a match on a frozen sub-run | `DispCall.v`, `SintCostR.v` |
| concrete whole-program runs | `refl` | **plain `reflexivity`** (`vm_compute` *explodes*) — the opposite dial | `rocq/README.md` |
| cost-count reshaping | `Data.Nat.Solver` + `subst` | `lia`/`nia` + cascade | `SintCost.agda` vs `SintCostR.v` |
| Cantor `unpair` | structural, no WF/fuel | structural, no WF/fuel — **same** | `NatPairing.{agda,v}` |
| `vnat` inverse `vunnat` | fuel = the number — **same** | fuel = the number — **same** | `ProgramDecode.{agda,v}` |
| int-efficient step bound | fuel-free **big-step cost relation** — **same** | big-step cost relation — **same** | `IntEfficient.agda` vs `IntEfficientR.v` |
| overhead constant `a` | **73** | **73** (equal) | `AGDA_CORRESPONDENCE.md`, `rocq/README.md` |

The one real divergence: the self-interpreter's per-node dispatch equations that Agda
closes definitionally by `refl` are exponential for Rocq's *kernel* `reflexivity`
once two/three sub-runs are substituted, and must be closed by `vm_compute` over a
**shape-faithful nested-match RHS** (a match on a frozen sub-run won't collapse); and
Rocq flips to plain `reflexivity` for concrete whole runs where `vm_compute` explodes
— a per-goal dial Agda does not have. The harder *structural* questions resolved
**identically** in both (Cantor unpairing structural; `vnat`-inverse needs fuel;
the linear bound needs a fuel-free big-step cost relation to keep sub-runs opaque),
landing on the same constant `a = 73`. Supporting: `docs/PROOFING-HEURISTICS.md`
(the D→A→B obstacle-naming method), `proofs/rocq/README.md` ("Proof-engineering
notes (hard-won)").

## ⑥ Framing: lead with the novel reversible payoff

The strongest single move: present the classical mechanization as the **reusable
substrate** from which a genuinely new theorem follows — **reversible Levin
optimality with a time↔garbage trade-off** (`RevLevinMain`: time-optimal at a proven
reversible constant AND garbage `|fibre| ≤ g ≤ |input|`; the first machine-checked
reversible-search optimality result, axiom-free, grounded to an actual reversible
machine `rsint`). By §② this is not a separate effort bolted on — it is Layer A
re-fired — so the classical + reversible results are one coherent contribution:
*a reusable, verified account of optimal search, instantiated classically (Jones)
and, for the first time, reversibly (novel).*

## The argument in the KC papers' idiom (for the intro)

> As Forster et al. render the classically-argued Kolmogorov-complexity results
> constructive and machine-checked, we render Jones's optimal-search proof
> axiom-free and machine-checked, and turn its hand-waved interpreter-overhead
> constant into a theorem (`a=73`); where their synthetic approach *assumes* a
> universal function, we *exhibit* Jones's concrete WHILE self-interpreter and bound
> its cost; and we verify the whole chain twice (Agda + Rocq). A faithful
> mechanization of a Kolmogorov/Levin-adjacent classical result is an accepted
> contribution at CPP/ITP (Catt–Norrish 2021; Forster et al. 2022); this is the
> first for Levin *optimal search*, and it composes into a novel reversible variant.
