# Reversible Levin R3 — decomposition plan (make `a-rev` a theorem)

R3 is the last, deep brick of the reversible Levin theorem (`docs/REV-LEVIN-SKELETON.md`):
turn the *measured* `a-rev ≈ 364×` into a *theorem* — a reversible self-interpreter with
a machine-checked linear overhead bound. That upgrades R0a's conditional time-optimality
to UNCONDITIONAL, completing "reversible Levin = classical Levin with a proven reversible
constant + provably unavoidable garbage" (a time↔garbage trade-off theorem — novel, no
prior formalization anywhere).

It mirrors the CLASSICAL chain, which is done and double-verified:
`UniversalInterp` (eval) → `SelfInterp` (`sint : Exp`) → `SintCost` (per-node costs) →
`IntEfficient` (`a = 73`). The reversible twins are R3a…R3d below.

**Status (2026-07-06): R3a+R3b DONE here (`proofs/agda/RevInst.agda`); R3c–R3e HANDED
OFF to `rwhile-C-ocaml`** (see `rwhile-C-ocaml/plan_reversible_levin_R3.md`) — the
reversible self-interpreter and its `a-rev` bound belong where the executable
reversible self-interpreter (`ri.rwhile`) and reversible-semantics Agda (`RWhileRev*`)
already live.  R3e feeds the proven constant back into `RevLevinAbstract` here.

## THE design blocker (resolved here)

`RevCore.Language`'s `atom : Upd → Cmd` carries `Upd = Σ (S→S) (involution)` — a
FUNCTION. Functions are not data, so `Cmd` is NOT encodable and admits no
self-interpreter. **Resolution:** R3 needs a CONCRETE, FIRST-ORDER reversible
instruction language `RCmd` — finite constructors, an ENCODABLE `⌜_⌝ : RCmd → V`
(like `Exp`/`ProgEncoding`) — whose atoms are named reversible ops (self-inverse or
inverse-paired), NOT arbitrary involutions. `RevCore` stays the *abstract* study
(inv-sound/cost-sym over any store); `RCmd` is the *concrete* object of the
self-interpreter.

## Bricks

- **R3a — a concrete encodable reversible language `RCmd`.** **DONE** (`RevInst.agda`):
  store = one tree variable, ops `swp` (swap the root's subtrees) / `sq` / `ifn` (branch
  on nil); `fwd`/`inv`/`bwd`, `inv-inv`, `preservesNil`, `inv-sound` (UNCONDITIONAL — no
  `WF`, since every op preserves nil-ness), `cost` + `cost-sym`.
- **R3b — encoding `⌜_⌝ : RCmd → V` + `decode`** (the `ProgEncoding` twin), `decode∘⌜⌝ = id`.
  **DONE** (`RevInst.agda`: `⌜_⌝`/`decode`/`decode-⌜⌝`).
- **R3c — the reversible self-interpreter** (the `sint`/`SelfInterpCorrect` twin).
  - **step 1 — DONE** (`RevSelfInterp.agda`): the self-interpretation ALGORITHM as a
    FUEL-INDEXED interpreter `usint : ℕ → V → V → V` that dispatches on the encoded op tag
    (the same tags `RevInst.decode` reads) and recurses on the sub-codes, plus the
    correctness theorem `usint-correct : size c ≤ n → usint n ⌜c⌝ s ≡ fwd c s`
    (`usint-exact` at exact fuel), and — for free from `RevInst.inv-sound` — reversibility
    `usint-reversible : usint … ⌜ inv c ⌝ (usint … ⌜ c ⌝ s) ≡ s`. Machine-checked on
    concrete instances (`module Examples`). This is the correct-dispatch core R3d counts.
  - **step 2 — IN PROGRESS**. Two prerequisites now done:
    - R1a'-guard (`RevLoop.agda`): the general reversible loop `from e do D loop L until f`
      + `inv-sound`.
    - **brick 1 — the object language `RCmdL.agda`** (DONE): an ENCODABLE, FIRST-ORDER
      reversible language with a loop -- atoms = `RevInst.RCmd` (run by the total `fwd`),
      guards = first-order `GCode` (nil-tests along hd/tl), `pSeq`/`pCond`/`pLoop`.
      `inv-sound` (atom case DEFINITIONAL from `RevInst.inv-sound`; loop case via the
      `RevLoop` `rev-rest` chain-reversal), `inv-inv`, `inv-complete`. All data ⇒
      encodable; this is the language `rsint` is written in.
    - **remaining (the crux term)**: write `rsint : RCmdL` -- the tag-dispatch-in-a-loop
      program realizing `RevSelfInterp.usint`, + its encoding `⌜_⌝ : RCmdL → V` (the R3b
      twin) and `rsint`-correct. Until then `usint` is the semantic self-interpreter (the
      `SelfInterp` twin) and `ri.rwhile` the executable cross-check.
- **R3d — reversible per-op cost + `a-rev` theorem** (the `SintCost`+`IntEfficient` twin).
  - **fuel-level — DONE** (`RevSintCost.agda`): a step count `usintC` for
    `RevSelfInterp.usint` (per-op dispatch constants swp=1/sq=3/ifn=4, reflecting tag-
    inspection depth) and a faithful object step count `ocost` (one step per executed
    RCmd node; `ifn` runs only the taken branch), with the KEYSTONE
    `int-efficient-rev : size c ≤ n → usintC n ⌜c⌝ s ≤ a-rev · ocost c s` (a-rev = 4).
    This is the `IntEfficient` (`a = 73`) twin at the fuel/structural level: the
    self-interpreter's overhead is provably LINEAR. Constant is small because this
    minimal language has O(1) tag dispatch.
  - **remaining (the 364× model)**: the measured `a-rev ≈ 364×`
    (`rwhile-C-ocaml/measure_ri_overhead.sh`) comes from `ri.rwhile`'s variable-list
    (`Vl`) walk, absent here. Bounding one op's interpretation cost by the `Vl` walk (a
    store-carrying interpreter) is the remaining work; and lifting the count from `usintC`
    to the eventual `rsint`'s own object-step count (R3c-step2 crux) reconciles the two
    cost models (per the skeleton's caveat).
- **R3e — close the loop:** a `LevinGrounded` twin with `overhead := a-rev`, feeding the
  proven constant into Layer A → `rev-levin-optimal` becomes UNCONDITIONAL. Then assemble
  with the garbage bracket (R0b/c) for the full time↔garbage trade-off statement.

## Reuse (already machine-checked, ready to plug in)

- `RevCore` (`inv-sound`, `cost`, `cost-sym`, `iterC`) — the reversibility + time-symmetry
  discipline `RCmd` follows; `cost-sym` gives forward=backward cost (so `a-rev` is a
  one-sided bound).
- `RevXor` (`xorSlot`, partial-involution `^=`) — the encodable tree-valued reversible
  atom (the R-WHILE `^=`), a good concrete atom for `RCmd`.
- `RevLevinAbstract` (`rev-levin-optimal` at abstract `a-rev`, garbage bracket) — R3e
  plugs the proven `a-rev` into this; the optimality superstructure needs no change.
- Classical `LevinBudget`/`NatPairing`/`LevinOptimal`/`LevinDovetail*`/`LevinComplete`
  — cost-model-agnostic, reused verbatim (6 of 11 layers, per REV-LEVIN-SKELETON).

## Risks / notes

- The reversible self-interpreter is the crux (R3c), as `sint` was classically. Keep
  `RCmd` MINIMAL (like `UniversalInterp`, not full R-WHILE) so `rsint` stays tractable.
- `a-rev`'s exact value is model-dependent (which reversible step count is canonical);
  the THEOREM is "linear overhead exists", the constant is secondary (as `a = 73` was).
- Sizing: classical B1.2 (`CostRelation`+`SintCost`+`IntEfficient`) was several modules;
  expect similar for R3d. R3a–b are S–M, R3c is L (the self-interpreter), R3d is L.
  Multi-session; each brick ends at a green `--safe` commit.

## What "R3 done" means

`int-efficient-rev` proven (a reversible self-interpreter with linear overhead `a-rev`),
fed into `RevLevinAbstract` so `rev-levin-optimal` is unconditional. Then the reversible
Levin theorem is complete: optimal within `4·a-rev·2^j·(T_q+T_r)` AND garbage
`|fibre| ≤ g ≤ |input|` — the first machine-checked reversible-search optimality result.
