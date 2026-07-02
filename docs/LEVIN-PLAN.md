# Proof plan — Levin's optimal search (Jones Thm 20.1.2)

A layered plan to machine-check Levin's theorem in `proofs/agda/`, reusing the
repo's proven pattern: prove the **abstract cost-model core** now (from the
already-verified budget lemmas), leave the two hard operational facts (efficient
interpreter, program enumeration) as **explicit hypotheses**, then discharge them
concretely for the I language — exactly how halting / recursion / the Futamura
meta-theorem were handled.

## Claimable scope (current state — read before writing any paper)

Machine-checked so far (`--safe`), and what may honestly be claimed:

- **DONE.** The combinatorial core (`LevinBudget`: `gsum<2last`, `overshoot`); the
  optimality inequality `gsum k < a_q·(T_q+T_r)`, `a_q = 4·a·2^j` (`LevinOptimal`,
  Layer A) — but **relative to** two abstract record fields, the interpreter overhead
  `a` (`overhead`) and q's index `j` (`idx`); program countability (`ProgramEnum`,
  B2a); and a total step-counting cost model (`StepCount`, B1.1).
- **DONE (keystone).** `int-efficient` (`IntEfficient`, B1.2) — the self-interpreter's
  LINEAR overhead `time_sint(⌜p⌝.(⌜p⌝.d)) ≤ 73·time_p(d)` is now MACHINE-CHECKED
  (Jones Thm 19.1.3), so the overhead constant `a = 73` is a proven fact, no longer an
  assumption. This discharges Layer A's `overhead` hypothesis.
- **Linked (Phase 4, `LevinGrounded`).** The proven `a = 73` is fed into Layer A's
  abstract record: `levin-grounded` is the optimality bound `gsum k < 4·73·2^j·(T_q+T_r)`
  with `overhead` no longer a hypothesis (`overhead-grounded : LinearOverhead 73 =
  int-efficient`).
- **Programs effectively enumerable (B2b, `ProgramDecode`).** The surjective decoder
  `enum : ℕ → Exp` with `enum-index : enum (index p) ≡ p` — `index` a section, `enum` a
  retraction. Both numbering directions are now machine-checked.
- **NOT done.** Only the operational `opt` and `time_opt = gsum k` (B3).

**Therefore the honest claim now is:** "we machine-check, in Agda `--safe`, that (i)
the combinatorial heart of Levin's optimal search holds; (ii) programs are countable;
and — the keystone — (iii) the one-variable self-interpreter has provably LINEAR
overhead (`a = 73`), so Levin's optimality bound holds relative only to the effective
program numbering and the operational `opt` assembly (B3), with the interpreter-cost
hypothesis discharged." The remaining gap to a fully self-contained theorem about
running I programs is B3 (build `opt`, tie `time_opt = gsum k`). Still do **not** claim
the *whole* Levin theorem is mechanized until B3 lands. A genuine methodological
contribution also stands: the **negative finding** that the `refl`-dispatch technique
mechanizing self-interpreter *correctness* (`simF`) does NOT transfer to *cost* — the
step count is trapped in a stuck sub-term — so verifying interpreter *efficiency*
needs the big-step cost relation (`CostRelation`) + derivation-tree dispatch costs
(`SintCost`), the route this repo now demonstrates end-to-end.

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

**B1.1 DONE — the step-counting semantics (`StepCount.agda`).** The reference
`eval`'s fuel is only a DEPTH bound (both branches share fuel `n`, so a depth-`d`
tree runs in fuel `d` but has up to `2^d` nodes), NOT the unit-cost `time_p`.
`evalT : ℕ → Exp → V → Maybe (V × ℕ)` threads a step counter, one per node SUMMED
over sub-evaluations, so `time` is the genuine total step count; `evalT⇒eval` ties
it to the reference value. This is the model `int-efficient` must be stated in --
and the reason `F ≤ a·G` on `simF`'s depth-fuel `G` is *false* (a `kcons` sums both
sub-costs while its depth-fuel does not grow), so B1 genuinely needs `evalT`.

**B1.2 remaining — the bound.**

> **FINDING (probe, 2026-07-02): `simF`'s refl-dispatch technique does NOT transfer
> to the step-counting model.** The `evalT` binds were made projection-based
> (eta-reducing, so a `just <symbolic pair>` no longer blocks) — a real improvement,
> committed — but the cost dispatch still will not close by `refl`. Reading the
> normal form of `time (12+F) (⌜khd e⌝-triple)` shows it stuck at sint's INNER
> self-interpretation of the *symbolic* `e`, wrapped in the step accounting
> (`bump`/`suc (ta+tr)`/nested `kifnil` on `SelfInterp.tagE`). Unlike the value-only
> `simF` (where the stuck sub-run `run (6+F) (e-triple)` appears verbatim on both
> sides and matches), the sub-run's step COUNT is trapped inside that stuck term, so
> the accounting cannot be rearranged into a closed `run (J+F)(e-triple) >>= λ r →
> just (hd (proj₁ r), C + proj₂ r)` form. The residual around the stuck sub-run is
> `bump`/nested, not a single `>>=`. So per-constructor `refl` cost-dispatch lemmas
> (the thing that made `simF` work) are unavailable here.

- **Consequence:** `simF-cost` cannot be a step-accounting *rewrite* of `simF`.
  A different structure is needed — the counts must be reasoned about with `≤` while
  a sub-run is still symbolic, which `refl`/`rewrite` cannot do.
- **Recommended strategy — a big-step COST RELATION.** Define inductive relations
  `⇓p : Exp → V → V → ℕ → Set` ("object `e` on `env` yields `v` in `c` steps") and
  the analogous sint-run relation, prove they agree with `evalT` (soundness/
  completeness vs the fuel), then prove `time_sint ≤ a · time_p` by induction on the
  object derivation: each object node contributes one step and a *bounded* block of
  sint steps, and `const + Σ a·cᵢ ≤ a·(1 + Σ cᵢ)` iff `const ≤ a`. The induction is
  on the relation (structural), so no sub-run ever needs to reduce under a symbolic
  argument. **L+** (build the relations + agreement + the bound). This is the honest
  path; the refl route is a dead end.
- Fallback: if the full bound stalls, `LevinOptimal` (Layer A) already gives Levin
  optimality *relative to* the overhead `a`; B1 remains the one open hypothesis.

#### B1.2 resolution plan (big-step cost relation) — detailed

The design that dodges the reduction obstacle: reason about a **derivation tree**, not
a reducing term. A sub-computation enters as an *opaque premise* (a proof), never as
an expression that Agda tries (and fails) to reduce under a symbolic argument.

New module `CostRelation.agda` (over `module _ (prog : Exp)`), then `IntEfficient.agda`.

**Phase 1 — the cost relation and its adequacy vs `evalT`.  [M, self-contained]**
- `data _⊢_⇓_#_ : Exp → V → V → ℕ → Set` (read `prog ⊢ e , env ⇓ v # c`), one rule per
  constructor mirroring `StepCount.evalT`'s counts:
  `⇓var : prog ⊢ var , env ⇓ env # 1`; `⇓khd : prog ⊢ e,env ⇓ v # c → prog ⊢ khd e,env
  ⇓ hd v # suc c`; `⇓kcons` sums `suc (ca+cb)`; `⇓kifnil-then/-else` pick a branch and
  add `suc (cc+cbranch)`; `⇓call : prog ⊢ e,env ⇓ arg # ca → prog ⊢ prog,arg ⇓ v # cv →
  prog ⊢ call e,env ⇓ v # suc (ca+cv)`.
- `complete : evalT prog G e env ≡ just (v,c) → prog ⊢ e,env ⇓ v # c` (induction on `G`,
  same `with … in` shape as `evalT⇒eval`).
- `sound : prog ⊢ e,env ⇓ v # c → Σ G, evalT prog G e env ≡ just (v,c)` (induction on the
  derivation; combine sub-fuels with `Mono.add-fuel`/`mono-≤`, take `G = 1 + max`).
- Result: `⇓` is the fuel-free characterisation of `time`, and derivations *compose*.

**Phase 2 — per-object-node sint dispatch cost, RELATIONALLY.  [L, the crux]**
- For each object constructor a lemma taking sint's sub-derivation(s) to sint's
  derivation for the whole node, with an explicit constant, e.g.
  `sint-khd : sint ⊢ (⌜p⌝∙(⌜e⌝∙env)) ⇓ v # cₑ → sint ⊢ (⌜p⌝∙(⌜khd e⌝∙env)) ⇓ hd v #
  (Kₖₕ𝒹 + cₑ)`.
- Proof = *construct* sint's derivation tree over its own dispatch code
  (`tagE`-walk on the CONCRETE tag `⌜khd e⌝` ⇒ pick the right `⇓kifnil` branch ⇒
  `ireq` builds the request `(⌜p⌝∙(⌜e⌝∙env))` by `⇓kcons`… ⇒ sint's `call` plugs in the
  premise `cₑ` via `⇓call` ⇒ `⇓khd` applies `hd`). `Kₙₒ𝒹ₑ` = the node count of that fixed
  tree (incl. the constant request-building cost; `⌜p⌝`/`env` are reached by projection,
  O(1) nodes, so `K` does not depend on program size).
- **De-risking:** do `khd` end-to-end FIRST to fix the technique and read off `Kₖₕ𝒹`;
  then `ktl`/`kcons`/`kifnil`/`call` by the same recipe. Semi-automation option: obtain
  the *concrete-tag prefix* of each derivation from `complete` run on the tag-walk
  (which is a closed computation), and glue the one symbolic sub-derivation with a small
  `⇓`-composition lemma — cuts the hand-built tree to just the `ireq`/`call` splice.
- Set `a = suc (max Kₙₒ𝒹ₑ)` (the `suc` covers the object node's own `+1`).

**Phase 3 — the linear bound, by induction on the object derivation.  [M]**
- `int-efficient : prog=p ⊢ e,env ⇓ v # c → Σ c', (sint ⊢ (⌜p⌝∙(⌜e⌝∙env)) ⇓ v # c') ×
  (c' ≤ a * c)`. Induct on the object derivation; each case applies the Phase-2 lemma to
  the IH sub-result(s), then the node inequality `K + Σ a·cᵢ ≤ a·(1 + Σ cᵢ)` — needs
  `K ≤ a` and `*`/`+` monotonicity (`*-distribˡ-+`, `+-mono-≤`, all in stdlib).
- Corollary `time_sint(⌜p⌝.d) ≤ a · time_p(d)` by `sound`/`complete` (Phase 1) at
  `e := p`, `env := d`.

**Phase 4 — discharge Layer A's hypothesis.  [S]  DONE (`LevinGrounded`).**
- `LevinGrounded.search j Tq Tr` builds a `LevinOptimal.LevinSearch` with
  `overhead := IntEfficient.a` (= 73), and `overhead-grounded : LinearOverhead 73 =
  int-efficient` witnesses that this exact constant is the machine-checked interpreter
  bound — so `levin-grounded` (= `levin-optimal` at that record) no longer *assumes* the
  overhead, it *is* `a = 73`. `aq (search 0 3 1) ≡ 292` checks the wiring. This is the
  payoff: Levin optimality with the interpreter-cost hypothesis fully discharged, `--safe`
  (still modulo `idx j` from B2b and B3's actual `opt` construction).

**Risk & effort.** Phase 1 is M and independently valuable (a fuel-free cost semantics).
Phase 2 is the L crux (hand-built derivations); the semi-automation via `complete` on the
concrete prefix is the key lever to keep it bounded. If Phase 2's exact `K` is painful,
a *generous* per-node bound (`K ≤ someLiteral`, proved by a coarser derivation count)
still yields linearity — exactness is not needed, only `K ≤ a`. Total: **L+**, but each
phase commits green on its own.

### B2. Program enumeration

An effective enumeration so each `q` gets a concrete index `j`, and (for the
dovetail) every program is reachable. Split into two halves — **both now DONE**.
(B2b turned out *easier* than the "well-founded and harder" first estimate: the
Cantor inverse `unpair` is structurally recursive via the diagonal-walk successor
recursions, so only `vunnat` needed a fuel argument.)

**B2a. Injective Gödel numbering `index : Exp → ℕ` — DONE.**
`ProgramEnum.index-injective` (+ `NatPairing.pair-inj`). Route `Exp --⌜_⌝--> V
--vnat--> ℕ`, both legs injective: `⌜_⌝` via `ProgEncoding.decode-⌜⌝` (left
inverse), `vnat` via the Cantor pairing `NatPairing.pair` on the two subtrees of
`V` (`pair-inj` proved by recovering the diagonal from the interval
`[tri d, tri (d+1))`). Gives `j := index q`, the constant `LevinOptimal.idx`
ranges over. Programs are countable. **DONE.**

**B2b. Total surjective decoder `enum : ℕ → Exp` with `enum (index p) ≡ p` — DONE
(`ProgramDecode`).** Invert the three legs of `index = vnat ∘ ⌜_⌝`:
- `NatPairing.unpair` — the inverse of the Cantor pairing. Turned out *structurally*
  recursive on ℕ (not well-founded on the pair value, as first feared): the pairing
  obeys the "diagonal walk" successor recursions `pair (suc x) y ≡ suc (pair x (suc y))`
  (`pair-suc-l`) and `pair 0 (suc x) ≡ suc (pair x 0)` (`pair-0-suc`), so `unpair (suc n)
  = unpair-step (unpair n)`. Round-trip `unpair-pair` via `unpair-correct` (pair ∘ unpair
  = id) + `pair-inj`.
- `vunnat` — inverse of `vnat`. Here `unpair n` is only *bounded* by `n`, so recursion is
  fuel-driven (`vunnat′ f`, `vunnat n = vunnat′ n n`); `vunnat′-vnat` shows `vnat v` fuel
  always suffices, using the component bounds `pair-fst-≤`/`pair-snd-≤`.
- `ProgEncoding.decode` — already the left inverse of `⌜_⌝` (`decode-⌜⌝`); out-of-range
  numbers default to `var`.
Composed: `enum-index : enum (index p) ≡ p`. `index` is a section, `enum` a retraction —
the effective numbering the B3 dovetail runs over. **DONE.**

### B3. Build `opt` + tie its cost to `gsum`  **[L]** — detailed plan in `docs/LEVIN-B3-DESIGN.md`

Design locked: phase-cost model **(A) batch** (each phase `i` costs exactly `pow2 i`,
so `time_opt = gsum k` reuses `LevinBudget`/`LevinOptimal`/`LevinGrounded` unchanged);
B3b (oracle correctness) **left as an explicit hypothesis first** (abstract-record
pattern); `opt` a **fuel-indexed Agda driver**, literal `Exp` version is stretch (B3d).
Order: B3a (`LevinDovetail`, cost skeleton, green) → B3c (`LevinComplete`, end-to-end
modulo oracle, green) → B3b (discharge oracle via `enum`/`unpair`/`sint`/`int-efficient`)
→ B3d (document). See the design doc for module/signature sketches.

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

1. **Layer A** — ✅ DONE (`LevinOptimal`), whole theorem modulo two named constants.
2. **B2 (enum)** — ✅ DONE (`ProgramEnum` B2a + `ProgramDecode` B2b); `index`/`enum`.
3. **B1 (int-efficient)** — ✅ DONE (`IntEfficient`, Thm 19.1.3, `a = 73`), the keystone,
   linked into Layer A by `LevinGrounded` (Phase 4).
4. **B3 (opt)** — the remaining long tail: build `opt` in `Exp`, prove `time_opt = gsum k`.

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
