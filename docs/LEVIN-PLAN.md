# Proof plan — Levin's optimal search (Jones Thm 20.1.2)

A layered plan to machine-check Levin's theorem in `proofs/agda/`, reusing the
repo's proven pattern: prove the **abstract cost-model core** now (from the
already-verified budget lemmas), leave the two hard operational facts (efficient
interpreter, program enumeration) as **explicit hypotheses**, then discharge them
concretely for the I language — exactly how halting / recursion / the Futamura
meta-theorem were handled.

## The theorem (informal, Jones §20.1)

For a semi-decidable search problem checked by a program `r` (given `x`, is `y` a
witness?), Levin's `opt` dovetails **all** programs under a doubling time budget,
simulating each by an efficient self-interpreter and checking outputs with `r`.
`opt` is **optimal up to a constant factor**: for every program `q` that solves
the problem,

> `time_opt(x) ≤ a_q · (time_q(x) + time_r(x, q(x)))`

where the constant `a_q` depends on `q` (its size / enumeration index and the
interpreter overhead) but **not** on the input `x`.

## What is already proved — `LevinBudget.agda`

- `gsum-eq` : `Σ_{i=0}^{k} 2^i = 2^{k+1} − 1` (exact geometric sum).
- **`gsum<2last`** : `gsum k < pow2 (suc k)` — the whole search up to phase `k`
  costs **< twice the last phase**.
- **`overshoot`** : for `N ≥ 1` some `2^k ∈ [N, 2N]` — the phase that first covers
  the target overshoots by at most a factor 2.

These are the two quantitative halves. What remains is (i) assembling them into
the optimality inequality, and (ii) supplying the operational constants they range
over.

---

## Layer A — the optimality inequality as an abstract meta-theorem  **[S–M, do now]**

New module `LevinOptimal.agda`. Bundle the operational facts as a record and prove
the inequality by pure arithmetic over `gsum<2last` + `overshoot`. This is the
mathematical heart of Levin and is fully tractable today.

```
record LevinSearch : Set where
  field
    a    : ℕ           -- efficient-interpreter overhead (real steps per simulated step), a ≥ 1
    -- per solver q, at input x:
    j    : ℕ           -- q's enumeration index  (constant in x)
    T_q  : ℕ           -- time_q(x)              (q halts)
    T_r  : ℕ           -- time_r(x, q(x))        (r halts)
  -- target real cost at which q's verified witness appears in the simulation:
  target : ℕ
  target = a * (T_q + T_r) * pow2 j
  -- opt's cost = the running total up to the phase whose budget first covers target
```

**Theorem A (`levin-optimal`).** There is a phase `k` with `target ≤ pow2 k` and the
total cost `gsum k` of phases `0..k` satisfies
`gsum k ≤ a_q · (T_q + T_r)` with `a_q = 4 · a · pow2 j`.

*Proof.* `overshoot target` gives `k` with `target ≤ pow2 k ≤ 2·target`. Then
`gsum k < pow2 (suc k) = 2·pow2 k ≤ 4·target = 4·a·pow2 j·(T_q+T_r)`. Set
`a_q = 4·a·pow2 j`. ∎  (All steps are `gsum<2last`, `overshoot`, and monotonicity
of `+`/`*` already in `Data.Nat.Properties`.)

Deliverable: `levin-optimal` green under `--safe`, plus a concrete numeric check
(e.g. instantiate `a,j,T_q,T_r` and evaluate `a_q`). This is Levin's theorem
*relative to* the two hypotheses `a` (interpreter overhead) and `j` (q's index) —
the same shape as `FutamuraProjections` (relative to `mix-eq`) or
`HaltingUndecidable` (relative to `build-diagonal`).

**Design choices to pin in Layer A**
- Schedule: phase `k` has budget `pow2 k`; q sits at index `j`, so q's real target
  cost is scaled by `pow2 j` (q only starts getting budget from phase `j`). This is
  the schedule `gsum`/`overshoot` already model. Document it in the module header.
- Cost model: `time` = fuel steps (the `UniversalInterp` fuel = the book's unit-cost
  `time_p`, matching `while --time`). Keep `time_q`, `time_r` as ℕ fields (q, r
  halt); only `opt`'s cost is derived.

---

## Layer B — discharge the hypotheses for the I language  **[the hard part]**

### B1. `int-efficient` (Jones Thm 19.1.3) — the constant `a`  **[L, keystone]**

`time_sint(⌜p⌝.d) ≤ a · time_p(d)` for a constant `a`: the one-variable self-
interpreter `sint` simulates any program with only constant per-step overhead.

- **Handle we already have:** `SelfInterpCorrect.simF` returns the sint-fuel `F` as
  an *additive* function of the object computation, with **bounded per-constructor
  constants** (khd `12`, ktl `13`, kcons `12`, kifnil `16`, call `18` + sub-costs).
  Those constants ARE the per-step overhead.
- **Task:** strengthen `simF` (or prove a companion `simF-cost`) to return, together
  with `F`, a proof `F ≤ a · G` where `G` is the object fuel and `a = max` of the
  per-constructor constants (plus the fixed dispatch walk). Induction on `G`, same
  case split as `simF`; each case adds a bounded constant, so the bound is linear.
- **Prereq:** agree that fuel = unit-cost steps (already the model). No new
  semantics needed; reuse `Mono.add-fuel`.
- Sub-steps: (a) define `a` explicitly; (b) per-constructor `≤` lemmas mirroring the
  dispatch lemmas; (c) assemble by induction. **M–L** given `simF`.

### B2. Program enumeration

An effective enumeration so each `q` gets a concrete index `j`, and (for the
dovetail) every program is reachable. Split into two halves; the estimate was
optimistic — only B2a is `S–M`, B2b is well-founded and harder.

**B2a. Injective Gödel numbering `index : Exp → ℕ` — DONE.**
`ProgramEnum.index-injective` (+ `NatPairing.pair-inj`). Route `Exp --⌜_⌝--> V
--vnat--> ℕ`, both legs injective: `⌜_⌝` via `ProgEncoding.decode-⌜⌝` (left
inverse), `vnat` via the Cantor pairing `NatPairing.pair` on the two subtrees of
`V` (`pair-inj` proved by recovering the diagonal from the interval
`[tri d, tri (d+1))`). Gives `j := index q`, the constant `LevinOptimal.idx`
ranges over. Programs are countable. **DONE.**

**B2b. Total surjective decoder `enum : ℕ → Exp` with `enum (index p) ≡ p`.**
The operational dovetail (B3) needs `enum` total and surjective — a well-founded
inversion of `vnat`/`pair` (`unpair`, decreasing on the pair value). **M–L** (the
round-trip + termination), heavier than first estimated; fold into B3.

### B3. Build `opt` as an actual `Exp` program + tie its cost to `gsum`  **[L]**

The operational specializer-free construction: `opt` = dovetail (`enum`) × budget
doubling (`pow2`) × run-via-`sint` × check-with-`r`, halting at the first verified
witness.

- **Task:** write `opt` in `Exp` (or as a fuel-indexed Agda driver mirroring it),
  and prove `time_opt(x) = gsum k` for the finishing phase `k` — i.e. connect the
  real running time of the operational `opt` to the abstract `gsum` of Layer A.
- This is the bridge from Theorem A's abstract `gsum k` to a concrete program's
  `time`. Depends on B1 (running via `sint`) and B2 (enum). **L.**

---

## Dependency graph & recommended order

```
LevinBudget (DONE) ──► [A] LevinOptimal (abstract inequality)      ← do next, green now
                          ▲            ▲
                          │            │
         [B1] int-efficient (a)   [B2] enum (j)      ← discharge the two hypotheses
                          ▲            ▲
                          └──── [B3] opt built in Exp, time_opt = gsum k
                                       │
                                       ▼
                       fully-concrete Levin (Thm 20.1.2)
```

1. **Layer A now** — immediate green, captures the whole theorem modulo two named
   constants (`a`, `j`). High value / low risk, and it fixes the exact interfaces
   B1/B2 must meet.
2. **B2 (enum)** — smallest concrete piece; gives `j` and a warm-up.
3. **B1 (int-efficient)** — the keystone; leverages `simF`'s per-constructor
   constants. Unlocks the real `a` and is independently citable (Thm 19.1.3).
4. **B3 (opt)** — assemble; the long tail.

## Notes / guardrails

- Keep every step `--safe` green; commit additively on `futamura-projections`;
  local commits only; run `gitleaks` before each commit; no `Co-Authored-By`.
- Mirror the repo idioms: abstract record + hypotheses (like `FutamuraProjections`,
  `HaltingUndecidable`), fuel = unit-cost steps, grid-search any stuck constant.
- Update `docs/PROOF-OBLIGATIONS.md` §4 and `docs/AGDA_CORRESPONDENCE.md` as each
  layer lands.
- `int-efficient` (B1) is the one genuinely hard cost proof; the rest is arithmetic
  and construction. If B1 stalls, Layer A + B2 already give "Levin optimal, relative
  to a linear-overhead interpreter" — a complete, honest result.
