# Rocq port of the Levin chain

A Rocq 9.1 port of the Agda Levin development (`proofs/agda/`). **Statement-level
fidelity**: theorem statements match the Agda modules; proofs are idiomatic Rocq
(`induction` + `lia`/`nia`, `Arguments … : simpl never` + unfold lemmas for the
interpreter, `vm_compute` for dispatch equations over the giant `sint`). Build/check:
`sh check.sh` (compiles every module in dependency order under a flat logical path via
`rocq compile -Q . ""`). All lemmas are axiom-free (`Print Assumptions` = *Closed under
the global context*).

The **Agda ↔ Rocq module correspondence** — which theorem each twin re-proves — is
tabulated in [`../../docs/AGDA_CORRESPONDENCE.md`](../../docs/AGDA_CORRESPONDENCE.md)
(§ *Rocq cross-check*). An executable check of the extracted interpreter lives in
`examples/desugar/test-extract.sh` (part of `run-all.sh`).

## Status — 22 modules; FULL G1 + int-efficient (a=73); Levin chain SELF-CONTAINED

| Rocq module | ports Agda | key content |
|---|---|---|
| `LevinBudget.v` | LevinBudget | pow2/gsum, gsum_lt_2last, overshoot(')/, pow2_add, monotonicity |
| `NatPairing.v` | NatPairing | pair/pair_inj, unpair/unpair_pair, bounds |
| `UniversalInterp.v` | UniversalInterp | V/Exp/eval, mono/add_fuel/eval_mono_le |
| `ProgEncoding.v` | ProgEncoding | enc/decode + decode_enc |
| `ProgramEnum.v` | ProgramEnum | vnat/index + index_injective |
| `ProgramDecode.v` | ProgramDecode | vunnat/enum + enum_index |
| `StepCount.v` | StepCount | evalT + evalT_eval |
| `CostRelation.v` | CostRelation | Steps + complete/sound/evalT_count/evalT_mono_le |
| `LevinOptimal.v` | LevinOptimal | LevinSearch, finish, levin_optimal |
| `LevinGrounded.v` | LevinGrounded | search (overhead:=73), levin_grounded/_solver |
| `LevinDovetail.v` | LevinDovetail | find/time_opt, find_least/find_complete/find_finish |
| `LevinDovetailRun.v` | LevinDovetailRun | Jones-stage oracle over abstract run + finishing |
| `LevinComplete.v` | LevinComplete | levin_complete / levin_operational |
| `SelfInterp.v` | SelfInterp | the concrete self-interpreter term `sint` + run checks |
| `SelfInterpCorrect.v` | SelfInterpCorrect (part) | `selfrun`, base dispatch (var/knil), guard/accessor lemmas |
| `SimF.v` | SelfInterpCorrect (part) | the 4 recursive dispatches (khd/ktl/kcons/kifnil) + call-free `sim` |
| `DispCall.v` | SelfInterpCorrect (part) | the `call` dispatch, offsets (20,10,13), shape-faithful nested RHS |
| `SimFFull.v` | SelfInterpCorrect `simF` | **`simF_full` — FULL G1** (object-fuel induction ∘ `disp_call`), unconditional |
| `SintCostR.v` | SintCost | cost dispatch, ALL 7: var 8 / knil 12 / khd 39 / ktl 48 / kcons 45 / kifnil 73 / call 64 — **equal to Agda** (a=73=max); cascade-RHS technique |
| `IntEfficientR.v` | IntEfficient | **`int_efficient` (a=73)** by `Steps`-induction + `int_efficient_time` (Thm 19.1.3) |
| `LevinRunFor.v` | LevinRunFor | `runFor` + `run_hits` + **`levin_optimal_run`** — `int_efficient_time` now DISCHARGED, no hypothesis |
| `Extract.v` | — (D1) | extraction: `while_eval.ml`, an executable OCaml interpreter = the verified `eval/sint`; exercised by `examples/desugar/test-extract.sh` (direct runs + self-interp oracles + G1 at runtime + `decode∘enc`) |

Highlights: `SimFFull.simF_full` is the ∀-quantified self-interpreter correctness
(Jones Thm 4.2.1 / the Agda `simF`) — for EVERY program incl. `call`/recursion.
`IntEfficientR.int_efficient` is the linear-overhead theorem (Thm 19.1.3, `a = 73`):
the self-interpreter reproduces any `Steps`-derivation's value within `73·c` steps.
`LevinComplete.levin_operational` is Levin optimality for the operational dovetail
(`gsum kop < aq*Swork`, `aq = 4·73·2^j`).  `LevinRunFor.levin_optimal_run` assembles
the concrete `sint`-driven search — now with its `int_efficient_time` hypothesis
DISCHARGED, so the whole chain is self-contained and axiom-free (`Print Assumptions
levin_optimal_run` = *Closed under the global context*).

Proof-engineering notes (hard-won): dispatch equations over `sint` close by
`vm_compute; reflexivity` (kernel reflexivity is exponential for multi-sub); the RHS
must be stated SHAPE-FAITHFULLY (nested option-matches for `call`, `bumpN` cost cascades
for the cost versions) with clean corollaries recovering the usable forms; concrete
full runs use plain `reflexivity` instead (`vm_compute` explodes on those).

## Phase 5 — DONE (cost / self-containment)

1. `SintCostR`: cost dispatch equations for all 7 constructors, constants 8/12/39/48/
   **45/73/64** — all EQUAL to Agda (`a = 73 = max`). The call cascade (hardest: a
   nested double interpretation) is `bumpN 12` on the inner run + two `+3` argenv-build
   wraps + `S` node + 6 return frames (5..10) = 64; found by diffing the `vm_compute`
   normal form against a shape-faithful candidate. ✓
2. `IntEfficientR`: `int_efficient` (a = 73) by induction on the `Steps` derivation,
   feeding each node's cost corollary + `evalT_mono_le`; `int_efficient_time` corollary. ✓
3. `LevinRunFor`'s `int_efficient_time` hypothesis DISCHARGED via `Require IntEfficientR`
   → the Rocq Levin chain is FULLY self-contained (only the solver's halting fact, the
   `Steps`/`chk` premises, remains as data). ✓

Everything axiom-free; `sh check.sh` compiles all 22 modules (~23s clean).
