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

- **R3a — a concrete encodable reversible language `RCmd`.** Fixed store (e.g. `V`, the
  single-variable I-language style, or a small register file), finite atomic reversible
  ops (swap-subtrees / rotate / the R-WHILE `^=` SET/CLEAR partial-involution from
  `RevXor`), `seq`, guard-preserving `cond`, bounded `iter`. `fwd`/`inv`/`bwd`,
  `inv-sound`, and step count + `cost-sym` (reuse the `RevCore` techniques). START:
  the datatype + `fwd`/`inv` + `inv-sound` on the atom+seq fragment.
- **R3b — encoding `⌜_⌝ : RCmd → V` + `decode`** (the `ProgEncoding` twin), `decode∘⌜⌝ = id`.
- **R3c — the reversible self-interpreter `rsint : RCmd`** (the `sint` twin): an `RCmd`
  program that interprets an encoded `⌜c⌝` over a store, reversibly. The hard core, like
  `universal.while`. First: dispatch on the encoded op tag; then the recursive cases.
- **R3d — reversible per-op cost + `a-rev` theorem** (the `SintCost`+`IntEfficient` twin):
  a reversible StepCount, per-op dispatch constants, then `int-efficient-rev :
  time_rsint ≤ a-rev · time_c` by induction on a reversible cost derivation. The measured
  `a-rev ≈ 364×` (`rwhile-C-ocaml/measure_ri_overhead.sh`) is the target ballpark and the
  design pointer is "bound one op's interpretation cost by the variable-list walk".
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
