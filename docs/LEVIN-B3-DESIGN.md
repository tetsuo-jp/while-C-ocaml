# Levin B3 — design for the operational `opt` and `time_opt = gsum k`

Status: **design only** (no code yet). This is the last piece of Jones Thm 20.1.2.
Layers A (`LevinOptimal`), B1 (`IntEfficient`, `a = 73`), B2a/B2b
(`ProgramEnum`/`ProgramDecode`, `index`/`enum`) are DONE and green; Phase 4
(`LevinGrounded`) links `a = 73` into Layer A. B3 builds the operational search and
ties its real cost to Layer A's abstract `gsum`.

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
B3a. The finishing phase `k` comes from `LevinOptimal.finish`/`overshoot` (already
proven) applied to the target `N = c · S`. This is the honest end statement:
*operational Levin optimality relative to the phase oracle*, `a = 73` a theorem.

### B3b — discharge the oracle with the real machinery  [M–L, the grind]

Instantiate `done` operationally and prove it satisfies B3c's premises. Uses every
prior layer:

```agda
-- phase i is a batch of candidate runs; unpair the batch coordinate into
-- (program index j, step budget t); run enum j on x via sint for ≤ t steps; check r.
run-candidate : (x : V) → ℕ × ℕ → Bool
run-candidate x (j , t) =
  case StepCount.evalT t sint (⌜ enum j ⌝ ∙ x) of        -- run program (enum j) on x
    just (y , _) → r x y                                  -- verify with the checker r
    nothing      → false

done : ℕ → Bool          -- true iff SOME candidate in phase i's batch verifies
```

Obligations to discharge B3c's premises:
- **Soundness:** `done i = true ⇒ Σ y, r x y ≡ true` (a genuinely verified witness).
- **Finishing-phase existence:** given the optimal solver `q` (index `j = index q`,
  `time_q x = Tq`, `r` verifies `q x` in `Tr`), there is a least `k` with
  `done k = true`, and `pow2 k` covers the need `N = c·S` via `overshoot`. The
  per-step interpreter overhead is absorbed by `int-efficient` (running `q` under
  `sint` costs `≤ 73·Tq`), and `enum (index q) = q` (from `ProgramDecode.enum-index`)
  guarantees `q` is actually reached by the dovetail.

Risks: aligning the `(j,t)` batch enumeration (via `NatPairing.unpair`) with the
abstract "`2^k ≥ c·S`"; making the batch's total budget exactly `pow2 i` (Cost model
A) while covering enough `(j,t)` pairs; and choosing input/interp conventions
(`⌜enum j⌝ ∙ x` vs the self-interp shape `⌜p⌝ ∙ (⌜e⌝ ∙ env)`) consistently with
`int-efficient`. This is why B3b is deferred behind the oracle hypothesis.

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
