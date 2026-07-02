# Levin B3 — design for the operational `opt` and `time_opt = gsum k`

Status: **B3a + B3b + B3c ALL DONE and green.** `LevinDovetail` (cost skeleton),
`LevinDovetailRun` (oracle over abstract runner), `LevinComplete` (end-to-end), and
`LevinRunFor` (the concrete `sint` runner: `runFor` + `run-hits` discharged from
`int-efficient-time` via `CostRelation.evalT-count`). `LevinRunFor.Solver.levin-optimal-run`
is the fully operational Levin search — halts at a stage whose real (phase-charged) time
`gsum kop < aq·(Tq+Tr)`, `a = 73` a theorem, **no hypotheses left but the solver's own
halting fact**. Only **B3d** (literal `opt ∈ Exp` step-counted, a stretch) remains, plus the
documented modelling scope (cost model (A); semantic checker `chk`; `opt` an Agda driver). This is the last piece of Jones Thm
20.1.2. Layers A (`LevinOptimal`), B1 (`IntEfficient`, `a = 73`), B2a/B2b
(`ProgramEnum`/`ProgramDecode`, `index`/`enum`) are DONE and green; Phase 4
(`LevinGrounded`, `search-solver`) grounds both `a = 73` and `idx = index q` into Layer
A. B3 builds the operational search and ties its real cost to Layer A's abstract `gsum`.

See `docs/LEVIN-PLAN.md` for the overall layering; this file is the B3-specific plan.

## Locked design decisions

1. **Phase-cost model = (A) batch.** Each phase `i` is a *batch* of candidate runs
   whose total budget is exactly `pow2 i`, so the cumulative cost over phases `0..k`
   is exactly `LevinBudget.gsum k` **by construction**. This reuses `LevinBudget`,
   `LevinOptimal`, and `LevinGrounded` **unchanged** — `time_opt = gsum k` plugs
   straight into `levin-grounded`. (Rejected: (B) redefining cost as raw dovetail
   step count, which would fork `gsum` and the geometric bound.)
2. **B3b left as an explicit hypothesis first.** B3a (the cost skeleton) and B3c
   (the end-to-end link) are proved green *relative to* a phase oracle
   `done : ℕ → Bool`; the oracle's operational correctness (B3b) is a well-scoped
   hypothesis, exactly the abstract-record pattern Layer A used before B1/B2 landed
   (cf. `FutamuraProjections` relative to `mix-eq`, `HaltingUndecidable` relative to
   `build-diagonal`). So the *whole B3 chain is green immediately*, with B3b as the
   single remaining discharge.
3. **`opt` is a fuel-indexed Agda driver, not a literal `Exp` program** (B3d). The
   mathematical content of Levin's theorem is the dovetail cost accounting +
   termination + oracle correctness — all captured by the driver. The literal WHILE
   program step-counted via `StepCount` is documented as stretch/out-of-scope.

## Sub-phases

### B3a — dovetail cost skeleton  [S–M, immediate green]

New module `LevinDovetail.agda`, parameterised by a phase oracle.

```agda
module LevinDovetail (done : ℕ → Bool) where
  -- least phase (from 0) at which `done` first holds, searched with fuel;
  -- returns the finishing phase index.
  find : ℕ → ℕ → Maybe ℕ         -- fuel, current-phase → finishing phase
  find zero    _ = nothing
  find (suc f) i = if done i then just i else find f (suc i)

  -- cumulative cost of phases 0..k is gsum k, BY CONSTRUCTION of the batch model.
  time-opt : ℕ → ℕ
  time-opt k = gsum k             -- (definitional; the batch of phase i costs pow2 i)
```

Key lemma (the "time_opt = gsum k" statement, oracle-relative):

```agda
-- if k is the LEAST phase with done k = true, the search finishes at k and its
-- total cost is exactly gsum k.
time-opt-eq : ∀ {k}
  → done k ≡ true
  → (∀ i → i < k → done i ≡ false)
  → Σ ℕ (λ f → find f 0 ≡ just k)          -- some fuel finds k
    × (time-opt k ≡ gsum k)                -- and its cost is gsum k
```

Content: `find` returns the least success (induction on the gap `k`, using the
"all earlier phases fail" premise); the cost identity is `refl` in the batch model.
Pure arithmetic + `LevinBudget`. No dependence on `Exp`/`sint`.

Provide a concrete non-vacuous instance: e.g. `done i = ⌊ 3 ≤? i ⌋`, check
`time-opt 3 ≡ 15` and `find 8 0 ≡ just 3`.

### B3c — end-to-end link  [S, green once B3a lands]

New module `LevinComplete.agda` (or a section of `LevinGrounded`). Combine B3a's
`time-opt-eq` with `LevinGrounded.levin-grounded`:

```agda
-- at the finishing phase k, the operational search's real cost is Levin-optimal.
levin-complete : ∀ j Tq Tr
  → (positivity + "k is the finishing phase for this solver" premises)
  → Σ ℕ (λ k → time-opt k ≡ gsum k × gsum k < aq (search j Tq Tr) * S (search j Tq Tr))
```

The `gsum k < aq·S` half is `levin-grounded`; the `time-opt k ≡ gsum k` half is
B3a. This is the honest end statement: *operational Levin optimality relative to the
phase oracle*, `a = 73` a theorem.

**Status: DONE** as `LevinComplete.levin-complete` (fixed `k_op = k`), green, with a
concrete non-vacuous instance. **B3b needs a `≤` refinement** (`levin-complete-≤` +
`gsum-mono`): the operational search's least success `k_op` may be *below* the abstract
bound phase `k` (a faster program found earlier — still optimal, `gsum k_op ≤ gsum k <
aq·S`). See the B3b section; that refinement is [S] and supersedes the fixed-`k` form.

### B3b — discharge the oracle with the real machinery  [M–L, the grind]

**IMPLEMENTED (over an abstract runner) — `LevinDovetailRun`, green.** The Jones
STAGE-model oracle (`hit`/`anyHit`/`done`), `done-sound`, `done-intro`, and `finishing`
(obligations 1 + 2 below) are all proven against an *abstract* `run : ℕ → Exp → V → Maybe V`
plus the single hypothesis `run-hits` (the solver reaches its output within any budget
`≥ 73·Tq`). A non-vacuity instance fires `finishing`. **So B3b is reduced to exactly one
remaining obligation: instantiate `run := runFor` (real `sint`) and prove `run-hits` from
`IntEfficient.int-efficient-time`** — an isolated StepCount lemma with no design risk (the
`runFor`-within-budget alignment; reuse the `CostRelation.sound`/`evalT-mono` pattern).
The design below records the intended `runFor` and the discharge; everything above it in
the chain is already green.

Refined design. Two corrections vs. the first sketch:
- **Dovetail structure = the Jones STAGE model, not `unpair`.** `unpair` enumerates a
  *flat* set of `(program, steps)` pairs; it does not carry the doubling phase
  structure the batch cost model (A) needs. The right shape is: **phase (stage) `i`
  runs programs `0..i`, giving program `m` a budget of `pow2 (i ∸ m)` sint-steps** —
  the classic diagonal-doubling dovetail. `unpair` is *not* used here (it belongs to
  B2b, already done). `NatPairing`/`ProgramDecode.enum` still provide the numbering.
- **`sint` convention.** Running program `p` on input `x` via the self-interpreter is
  `sint` on the triple `(⌜p⌝ ∙ (⌜p⌝ ∙ x))` (SelfInterp: program, current-expr = whole
  body, env = x). `int-efficient-time` then bounds its step count:
  `StepCount.evalT _ sint (⌜p⌝ ∙ (⌜p⌝ ∙ x))` costs `≤ 73 · time_p(x)`.

**Checker.** Model the verifier semantically as a parameter `chk : V → V → Bool`
(`chk x y = true` ⟺ `y` solves `x`). *First cut: `chk` is "free"* (not step-charged),
so `S = Tq + Tr` is used only through `Tq` (loose but sound). *Faithful refinement:*
also run `r` via `sint` inside the budget, needing budget `≥ 73·(Tq+Tr)`; then `Tr`
enters genuinely. Do the free version first.

**Definitions** (module parameterised by `chk : V → V → Bool` and input `x : V`):

```agda
-- run program p on x via sint; return its output iff it halts within `b` sint-steps.
runFor : ℕ → Exp → V → Maybe V
runFor b p x with StepCount.evalT b sint (⌜ p ⌝ ∙ (⌜ p ⌝ ∙ x))
... | just (y , c) = if c ≤ᵇ b then just y else nothing   -- (fuel = b ≥ depth since steps ≤ b)
... | nothing      = nothing

hit : ℕ → ℕ → Bool                    -- level i, program m: enum m verifies within pow2(i∸m) steps
hit i m with runFor (pow2 (i ∸ m)) (enum m) x
... | just y  = chk x y
... | nothing = false

anyHit : ℕ → ℕ → Bool                 -- level i; scan programs 0..bound
anyHit i zero    = hit i 0
anyHit i (suc m) = hit i (suc m) or anyHit i m

done : ℕ → Bool
done i = anyHit i i                    -- programs 0..i at stage i
```

**Cost note.** The literal sint-step work at stage `i` is `Σ_{m≤i} pow2(i∸m) = gsum i`
(≈ `2·pow2 i`); cost model (A) *charges* `pow2 i` per phase. So `time_opt = gsum k` is
the phase-charged model cost, faithful to the real dovetail up to a factor 2 — a
constant already absorbed into `a_q`. State this explicitly in `LevinDovetail` /
the paper; it is standard for Levin bounds ("optimal up to a constant `a_q`").

**Obligations** (feed `levin-complete`):

1. **Soundness** `done i ≡ true → Σ[m] Σ[y] (runFor (pow2 (i∸m)) (enum m) x ≡ just y) ×
   (chk x y ≡ true)`. Bool induction on `anyHit`: a `true` `or` yields a hitting `m ≤ i`;
   `hit i m = true` unfolds to `runFor … ≡ just y` with `chk x y ≡ true`. **[S]**

2. **Finishing at stage `k`** — the substantive coupling. Given the optimal solver
   with index `j` (so `enum j = q` by `enum-index`), `time_q(x) = Tq`, output `yq`,
   `chk x yq ≡ true`:
   - `int-efficient-time` (+ a StepCount fuel/step lemma like `CostRelation.sound`)
     gives `sint` halting on `(⌜q⌝ ∙ (⌜q⌝ ∙ x))` in `cₛ ≤ 73·Tq` steps with output `yq`.
   - `overshoot (73·Tq ∸ 1)` gives an offset `d` with `73·Tq ≤ pow2 d ≤ 2·73·Tq`.
   - Set `k = j + d`. Then `pow2 (k ∸ j) = pow2 d ≥ 73·Tq ≥ cₛ`, so `runFor (pow2 (k∸j))
     (enum j) x ≡ just yq` (budget suffices), hence `hit k j ≡ true`, hence `done k ≡ true`.
   - **Bound:** `pow2 k = pow2 j · pow2 d ≤ pow2 j · 2·73·Tq = 146·2^j·Tq`, so
     `gsum k < 2·pow2 k ≤ 292·2^j·Tq ≤ 292·2^j·(Tq+Tr) = a_q·S`. So `gsum k < a_q·S`.
   **[M–L]** — the arithmetic is `overshoot` + `pow2`-homomorphism `pow2 (j+d) =
   pow2 j · pow2 d` + monotonicity; the operational core is the `runFor`-succeeds-within-
   budget lemma (int-efficient ∘ StepCount).

3. **Operational finish is `≤ k`, not `= k`** — **DONE (sub-phase 1, green).** Levin does
   NOT claim `q` is the *first* program found — a faster program may verify at an earlier
   stage `k_op < k`, which is *even better* (`gsum k_op < gsum k < a_q·S`). So the
   operational finish `k_op` satisfies only `k_op ≤ k`. Implemented (cleaner than the
   first `least-upto` + `levin-complete-≤` sketch):
   ```agda
   -- LevinBudget:
   gsum-mono : ∀ {m n} → m ≤ n → gsum m ≤ gsum n            -- via pow2-mono / 1≤gsum
   -- LevinDovetail.Search (characterises the ACTUAL find):
   find-complete : ∀ f i k → i ≤ k → done k ≡ true → suc k ≤ i + f
     → Σ k' , (find f i ≡ just k') × (k' ≤ k) × (done k' ≡ true)   -- first success, k' ≤ k
   find-finish : ∀ k → done k ≡ true
     → Σ kop , (Σ f , find f 0 ≡ just kop) × (kop ≤ k) × (done kop ≡ true)
   -- LevinComplete (supersedes the fixed-k `levin-complete`):
   levin-operational : ∀ j Tq Tr {k} → gsum k < aq·S → done k ≡ true
     → Σ kop , (Σ f , find f 0 ≡ just kop) × (time-opt kop ≡ gsum kop) × (gsum kop < aq·S)
   ```
   `find-complete` bundles the bound `k_op ≤ k` (so no separate leastness extraction is
   needed); `levin-operational` takes only `done k ≡ true` + the Layer-A bound at `k` —
   exactly what B3b's finishing lemma produces. Concrete instance fires it. **DONE.**

**Discharge chain:** `q` solves `x` → (2) `done k ≡ true` with `gsum k < a_q·S` [B3b
finishing lemma, the remaining grind] → (3, done) `levin-operational` → `time_opt =
gsum k_op < a_q·S`, fully operational, `a = 73` a theorem. **So B3b is reduced to
obligations (1) soundness and (2) the finishing lemma alone.**

**Residual risks:** the `runFor`-within-budget lemma (aligning `int-efficient-time`'s
`≤ 73·Tq` with `evalT`'s fuel-as-depth vs steps-returned — reuse the `evalT-mono`/`sound`
pattern from `CostRelation`); and, for the faithful checker, threading `r`'s sint-cost
into the budget. The stage-model / cost-model-(A) factor-2 gap is documented, not proved
away (it lives in `a_q`).

### B3d — stretch (out of scope): `opt ∈ Exp`

The literal WHILE program (dovetail loop + Cantor pairing coded in `Exp` + budget
bookkeeping + `sint` invocation), step-counted via `StepCount.evalT`, with
`time_opt` the real step count. Documented as future work; the Agda driver above
already discharges the mathematical content of Thm 20.1.2.

## Recommended order

**B3a (green, cost theorem) → B3c (green, end-to-end modulo oracle) → B3b (grind,
oracle discharge) → B3d (document).** Each of B3a/B3c ends at a green commit; the
high-risk B3b is last and isolated behind a single hypothesis.

## What "done" means for the paper

After B3a + B3c: *"We machine-check, in Agda `--safe`, Levin's optimal-search
inequality for an operational dovetailing search whose real running time is exactly
`gsum k`, with the linear interpreter overhead `a = 73` and the program numbering
both proven — relative to a phase oracle whose correctness is the sole remaining
operational hypothesis."* After B3b: the oracle hypothesis is discharged and the
search is fully concrete (modulo `opt` being an Agda driver rather than a literal
`Exp` program, B3d). Do not claim the *whole* Thm 20.1.2 is mechanized until at
least B3b lands.
