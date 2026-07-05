# Reversible Levin search — formalization skeleton (the R-track)

**Status: R0 done** (`proofs/agda/RevLevinAbstract.agda`, `--safe`, green). Everything
below R0 is design. This file is the **canonical** R-track document; the reversible
sibling's receiving end (role split + the `a-rev` measurement task on `ri.rwhile`) is
`rwhile-C-ocaml/plan_reversible_levin.md` — do not fork the design there. The classical track this builds on is complete: Jones Thm 20.1.2
mechanized in Agda (58 modules; B3d = literal `opt ∈ Exp` remains a documented stretch)
and independently in Rocq (22 modules, axiom-free), with the interpreter overhead
`a = 73` a theorem in both (`IntEfficient` / `IntEfficientR`).

## The question

Levin's theorem: the budget-doubling dovetail search solves any search problem within
`a_q·(T_q + T_r)` of ANY solver `q` — optimal up to a per-solver constant. The
reversible question: **does an R-WHILE-style (reversible) search program achieve the
same optimality, and at what garbage cost?** No formalization of this exists anywhere
in the project (grep: zero mentions of Levin in `rwhile-C-ocaml`/`rev_PE`); the
question itself appears to be open in the literature in this mechanized form.

Two known forces pull in opposite directions:

- **Bennett (upper bound / feasibility).** Any classical run can be simulated
  reversibly at constant time overhead (run forward, copy the result, run backward),
  keeping the input. So *time*-optimality should survive with a bigger constant —
  reversible Levin = Levin with `a-rev` in place of `a = 73`.
- **Theorem A (`RevSpecGarbage`, lower bound / impossibility).** The search's witness
  map `wit : instance → witness` is typically non-injective (many instances share a
  witness), so **no clean (garbage-free) reversible search exists**; the reversible
  state must retain ≥ log |fibre| bits beyond the witness. Optimality and cleanliness
  are jointly unachievable.

So the theorem to aim for is a **pair**: time-optimality (with a proven reversible
constant) + a garbage bracket — the reversible Levin search is exactly a
*time-garbage trade-off* statement, which is the novel content beyond Jones.

## Cost model decisions (fix these before proving)

1. **Time** = unit-cost steps of the reversible operational semantics (the twin of
   `StepCount.evalT`), same batch cost model (A) as the classical track.
2. **Garbage** = the part of the reversible searcher's (injective) output that the
   decode discards — Theorem A's `d : G → Y` reading. The searcher computes
   `x ↦ (witness, garbage)`; the claim's decode is the witness projection.
3. **Competitor class**: classical solvers `q` (the search problem is classical; only
   the *searcher* is reversible). This matches Jones's statement and is the harder,
   more meaningful variant — the searcher must reversibly simulate classical candidate
   runs (Bennett). The reversible-solver variant (`q` itself reversible) is strictly
   easier (no Bennett on the solver side) and can be a warm-up corollary.

## Candidate theorems

- **R1 (reversible Levin time-optimality).** There is a reversible search program
  `r-opt` such that for every solver `q` (index `j`) halting on `x` in `T_q` steps
  with checker time `T_r`:  `time_r-opt(x) < 4 · a-rev · 2^j · (T_q + T_r)`, where
  `a-rev` = the reversible-simulation overhead (reversible self-interpreter × Bennett
  factor). **R0a shows Layer A fires verbatim the moment `a-rev` is a theorem** — the
  whole optimality superstructure is cost-model agnostic (reuse table below).
- **R2 (garbage bracket).** Lower: a non-trivial search (two instances sharing a
  witness) has no clean reversible realization — **R0c, done** (Theorem A applied to
  the witness map, with a concrete non-vacuous instance). Upper: the Bennett
  realization keeps the input — **R0b, done at the value level** (`⟨id, wit⟩`
  injective, decodes by `proj₂`, instantiates Theorem A's `Realization`). So
  `|fibre(wit, y)| ≤ garbage ≤ |input|` (+ retained search control, see risks).
  The R-track-specific quantitative target: the garbage of the *optimal* searcher —
  conjecture: input + O(log k) (uncompute each failed candidate Bennett-style inside
  its phase; retain only the finishing phase/machine pair).
- **R3 (the reversible overhead constant).** The reversible twin of `a = 73`: a
  reversible self-interpreter with machine-checked linear overhead. `rwhile-C-ocaml`
  has an *executable* reversible self-interpreter (`examples/ri.rwhile`) but no cost
  semantics and no cost proof — this is the genuinely new formal work. Classical
  precedent for effort: B1 took `CostRelation` + `SintCost` + `IntEfficient`, and the
  Rocq re-proof established the cascade/`vm_compute` technique that transfers.
  **MEASURED (2026-07-05, `rwhile-C-ocaml/examples/measure_ri_overhead.sh`):** in
  R-WHILE's command-step model, `a-rev ≈ 364×` (mean over 5 non-trivial subjects,
  range 228–466), and — crucially — *roughly constant* across program size
  (perm_to_code at 4097 steps still ~437), the empirical analogue of `a` being a
  per-step constant. The variation tracks how often each subject triggers
  `ri.rwhile`'s `LOOKUP`/`UPDATE`, each a linear walk of the ~75-nil variable list
  `Vl`. **Design pointer for the R3 proof:** bound one command's interpretation cost
  by the `Vl` walk — that is where the `a-rev` constant comes from, the reversible
  analogue of the classical per-node dispatch constants (khd 39 … call 64). Caveat:
  this is R-WHILE's command-step count, not the node-summed `evalT` model `a = 73`
  lives in; R3 must fix which count is the reversible canonical one (likely
  node-summed) before the two constants are comparable.

## Reuse map (classical chain → reversible chain)

| classical module | reversible reuse |
|---|---|
| `LevinBudget` (gsum/pow2/overshoot) | **verbatim** — pure arithmetic |
| `NatPairing` | **verbatim** |
| `LevinOptimal` (Layer A) | **verbatim** — only the `overhead` *field* changes (R0a, done) |
| `LevinDovetail` (B3a cost skeleton) | **verbatim** — abstract over `done : ℕ → Bool` |
| `LevinDovetailRun` (B3b oracle) | **verbatim** — abstract over `run` (:= the reversible runner) |
| `LevinComplete` (B3c assembly) | **verbatim** |
| `RevSpecGarbage` (Theorem A) | **verbatim** — supplies the no-clean leg (R0c, done) |
| `ProgramEnum`/`ProgramDecode` | adapt — enumerate *reversible* programs (same pairing technique) |
| `StepCount`/`CostRelation` | adapt — reversible semantics + step count (R1b) |
| `SelfInterp`/`SimF`/`SintCost`/`IntEfficient` | **new** — reversible sint + dispatch constants ⇒ `a-rev` (R3) |
| — | **new** — the Bennett TIME lemma (reversible simulation of a classical run at constant factor, input kept) (R1c) |

The striking consequence, pinned by R0a: **six of the eleven classical layers are
reusable verbatim**. Reversible Levin reduces to (i) `a-rev` (R3), (ii) the Bennett
time lemma (R1c), (iii) the garbage accounting (R2a) — everything else is plumbing
already built and double-checked (Agda + Rocq) on the classical side.

## First lemmas (test-first order, each independently green-able)

| step | content | size |
|---|---|---|
| R0 | **DONE** — `RevLevinAbstract.agda`: Layer A at abstract `a-rev`; Bennett value-level realization; no-clean for merging witness maps + non-vacuity | — |
| R1a | **DONE** — `RevCore.agda`: minimal reversible language `Language S` (self-inverse `Upd` = the algebra of `X ^= E`, `seq`, guard-preserving reversible `cond`), `fwd`/`inv`/`bwd`, `inv-inv`, keystone `inv-sound : WF c → bwd (fwd s) ≡ s`; concrete swap-based tree instance, round-trips by `refl` | M |
| R1a' | loops (`from E do C loop D until F`, exit assertions + termination) + the GENERAL exit-assertion `cond` (partial/`Maybe` semantics, reversibility on defined runs); full bijection (`fwd ∘ bwd = id` too) | M |
| R1b | **DONE** — `RevCore.agda`: `cost`/`fwdT` (unit-cost executed-command count, the `StepCount.evalT` twin for a total semantics), `fwdT-value` (same store), and the keystone **`cost-sym : WF c → cost (inv c) (fwd c s) ≡ cost c s`** (the backward run costs EXACTLY the forward run — reversible time-symmetry, the operational content of "uncomputing is as cheap as computing") | S–M |
| R1c | **Bennett lemma in REXP**: for each classical `Exp` program `p` a reversible `B(p)` with `run (B p) x = (x , ⟦p⟧ x)` (value level first), then `time_{B(p)} ≤ c_B · time_p + c'·|copy|` (the real content) | M–L |
| R2a | garbage of the concrete searcher: make "retain (k, m), uncompute failed runs" precise; connect to `fibre-map` | M |
| R3a | reversible self-interpreter for REXP + `SintCost`-style dispatch constants ⇒ `a-rev` a theorem; then a `LevinGrounded` twin with `overhead := a-rev` closes R1 | L (the crux) |

Recommended order: R1a → R1b → R1c(value) → R3a → R1c(time) → R2a → assembly.
A Rocq twin of the R-track is natural once R3a lands (the `vm_compute` cascade
technique from `SintCostR`/`IntEfficientR` transfers directly).

## Risks / open design points

- **The Bennett time constant.** Forward + copy + backward ≈ 2×time + |copy|; the
  copy cost must fit Levin's `(T_q + T_r)` budget shape. Likely absorbed into
  `a-rev`, but the constant bookkeeping needs the same care as the classical
  khd/ktl/kcons/kifnil/call dispatch constants did.
- **Failed candidate runs.** Uncomputing each failed run doubles the per-phase cost —
  a constant absorbed by `gsum`'s doubling. But the search *control* (which phase
  `k`, which machine `m` hit) cannot be uncomputed — it is genuinely produced
  information. Make "garbage ≥ the finishing pair" precise in R2a; this is where the
  reversible statement genuinely differs from Jones's.
- **Language choice.** Define `REXP` fresh in Agda (self-contained, small, like
  `UniversalInterp` — NOT a model of full R-WHILE syntax). `rwhile-C-ocaml`'s
  `ri.rwhile` stays the *executable cross-check* (differential), mirroring this
  repo's proven-vs-tested methodology.
- **Reversibility of the searcher vs. of the search *function*.** The searcher
  program is injective on its whole state; the search function `wit` is not. Keep
  Theorem A's realization framing (`g` injective, `d` decodes) — R0b/R0c already
  live entirely in that framing, no new notion needed.

## What "done" means

"**Reversible Levin = Levin with a proven reversible constant, plus unavoidable
garbage**": R1 (time-optimality, `a-rev` a theorem) + R2 (garbage bracket) mechanized
relative to the same modelling scope as the classical track (batch cost model (A),
semantic checker `chk`, driver-level `opt`). Do not claim the theorem before R3a
lands — before that, the honest statement is R0's: "time-optimality is reduced to
the reversible overhead constant; cleanliness is already refuted."
