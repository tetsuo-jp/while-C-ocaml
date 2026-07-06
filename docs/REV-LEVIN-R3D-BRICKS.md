# R3d/R3e remaining — decomposition into small bricks

Companion to `REV-LEVIN-R3-PLAN.md` (kept separate because that file is under active
parallel edit; merge this in when it settles).  Baseline (2026-07-06, all green,
axiom-free): `RevSelfInterp` (usint + correctness), `RevLoop` (R1a'-guard), `RCmdL` +
`RCmdLEnc` (encodable loop language + encoding), and **`RevSintCost.int-efficient-rev`
(a-rev = 4, fuel level) — the driver-level linear-overhead theorem is PROVEN.**

What remains splits into three INDEPENDENT tracks.  Two deliberate abstractions make
them tractable (abstraction explicitly allowed):

- **(i) a generic linear-overhead lemma** — parameterise the per-op interpretation
  charge `κ : Op → ℕ` by a bound `κ ≤ K` and prove `icost ≤ suc K · ocost` ONCE,
  purely arithmetically; every concrete model (meta-dispatch a-rev=4, store-walk,
  program-level) becomes an instantiation.
- **(ii) an abstract store interface** — the measured 364×'s entire content is
  "lookup/update = a walk of the length-L variable list"; provable with no interpreter.

## Track 1 — R3e at the driver level (UNBLOCKED NOW; finishes the theorem statement)

| brick | content | size |
|---|---|---|
| E1 | `RevLevinGrounded.agda` (the `LevinGrounded` twin): `LinearOverheadRev ov = ∀ c s n → size c ≤ n → usintC n ⌜c⌝ s ≤ ov * ocost c s`; `overhead-grounded : LinearOverheadRev a-rev := int-efficient-rev`; `rev-search j Tq Tr` with `overhead := a-rev`; `rev-levin-grounded` = `RevLevinAbstract.rev-levin-optimal` at that record → `gsum k < 4·a-rev·2^j·(Tq+Tr)` with the constant a THEOREM | S |
| E2 | the trade-off package: one record bundling `rev-levin-grounded × no-clean-rev-search × Bennett-upper` (R0b/R0c) + a non-vacuity instance = "**reversible Levin, driver level**: time-optimal at a proven constant AND garbage `\|fibre\| ≤ g ≤ \|input\|`"; docs updated with the honest scope (interpreter = the semantic `usint`; program level = Track 3) | S |

After E2 the reversible Levin theorem is CLAIMABLE (driver level).  Tracks 2–3 are
fidelity upgrades — exactly the relation classical B3d bears to Thm 20.1.2.

## Track 2 — the 364× (store-carrying) model: a-rev is AFFINE in the store size

| brick | content | size |
|---|---|---|
| S1 | `RevStore.agda`: store = association list of length `L`; `lookup`/`update` **with cost = walk length**; `lookupC ≤ L`, `updateC ≤ L` (the `Vl`-walk content, interpreter-free — abstraction (ii)) | S |
| S2 | `RevLinearOverhead.agda` (abstraction (i)): per-op charges `κ op ≤ K` ⇒ `icost ≤ suc K · ocost`; extract the arithmetic of `RevSintCost.int-efficient-rev` once, reuse thrice | S |
| S3 | `RCmdV`: multi-variable object language (ops take a variable index; `fwd` over the store; `ocostV` = 1 per node) | M |
| S4 | store-carrying interpreter `usintV` + count `usintVC`: each dispatched op charges dispatch + lookup-walk + update-walk; **per-op bound `κ op ≤ k + 2·L`** — the measurement's design pointer ("bound one op by the `Vl` walk") as a lemma | M |
| S5 | instantiate S2 with S4: `int-efficient-rev-store : … ≤ suc (k + 2·L) · ocostV` — **`a-rev(L)` affine in the store size**; at `L ≈ 75` this is the measured ≈364× shape (order-of-magnitude doc note) | S |

## Track 3 — `rsint` (program level) + cost reconciliation

The crux term, decomposed by **reversible defunctionalization**: the recursion `usint`
gets from fuel becomes an AGENDA LOOP — state `(todo-stack, done-stack, store)`; each
iteration pops a tag from `todo`, applies the op / pushes sub-codes, and moves the
processed code to `done`.  The move keeps every step injective: **the `done` stack IS
the Bennett garbage** (R0b made concrete — exactly why `ri.rwhile` is
program-preserving, outputting `(⌜p⌝ . result)`).

| brick | content | size |
|---|---|---|
| P1 | `costL` — the loop language's own step count (incl. loop iterations, via the `RevLoop` chain) | S–M |
| P2 | the META agenda machine: `astep : todo × done × store → …` + `agenda-run ≡ usint` (defunctionalization correctness; fuel-free by todo-size induction) | M |
| P3 | the agenda step as a loop-language body (`GCode` guards on the tag; loop via `pLoop`); `rsint`; `rsint-correct` from P2 + the `RevLoop` unrolling lemma | M–L |
| P4 | iteration bound `#iterations ≤ c₁·ocost` + per-iteration `costL(body) ≤ \|body\|` ⇒ **`costL rsint … ≤ a-revL · ocost`** — the program-level `a-rev` | M |
| P5 | reconciliation: `usintC` a-rev=4 (meta dispatch) vs `a-rev(L)` (store walk) vs `a-revL` (program) vs measured 364 — fix the CANONICAL count in the skeleton | S |

## Dependencies / order

- Track 1: **now** (nothing new needed).
- Track 2: S1 ‖ S2 ‖ S3 → S4 → S5.
- Track 3: P1 ‖ P2 → P3 → P4 → P5.
- Each brick ends at a green `--safe` commit.  Recommended: E1→E2 first (theorem
  claimable), then S2 (shared lemma), then whichever of Track 2/3 the session favours.
