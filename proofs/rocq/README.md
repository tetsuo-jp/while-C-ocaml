# Rocq port of the Levin chain

A Rocq 9.1 port of the Agda Levin development (`proofs/agda/`). **Statement-level
fidelity**: theorem statements match the Agda modules; proofs are idiomatic Rocq
(`induction` + `lia`/`nia`, `Arguments … : simpl never` + unfold lemmas for the
interpreter, `vm_compute` for dispatch equations over the giant `sint`). Build/check:
`sh check.sh` (compiles every module in dependency order under a flat logical path via
`rocq compile -Q . ""`). All lemmas are axiom-free (`Print Assumptions` = *Closed under
the global context*).

## Status — 21 modules; FULL self-interpreter correctness (G1) proven

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
| `SintCostR.v` | SintCost (pilot→) | cost dispatch: var 8 / knil 12 / khd 39 / ktl 48 — **equal to Agda**; cascade-RHS technique |
| `LevinRunFor.v` | LevinRunFor | `runFor` + `run_hits` + `levin_optimal_run`, modulo `int_efficient_time` hypothesis |
| `Extract.v` | — (D1) | extraction: `while_eval.ml`, an executable OCaml interpreter = the verified `eval/sint` |

Highlights: `SimFFull.simF_full` is the ∀-quantified self-interpreter correctness
(Jones Thm 4.2.1 / the Agda `simF`) — for EVERY program incl. `call`/recursion.
`LevinComplete.levin_operational` is Levin optimality for the operational dovetail
(`gsum kop < aq*Swork`, `aq = 4·73·2^j`).  `LevinRunFor.levin_optimal_run` assembles
the concrete `sint`-driven search, currently modulo the `int_efficient_time` hypothesis.

Proof-engineering notes (hard-won): dispatch equations over `sint` close by
`vm_compute; reflexivity` (kernel reflexivity is exponential for multi-sub); the RHS
must be stated SHAPE-FAITHFULLY (nested option-matches for `call`, `bumpN` cost cascades
for the cost versions) with clean corollaries recovering the usable forms; concrete
full runs use plain `reflexivity` instead (`vm_compute` explodes on those).

## Remaining — Phase 5 (cost) to full self-containment

1. `SintCostR`: cost dispatch equations for kcons/kifnil/call (expected constants
   45/73/64, = Agda). **S–M** (cascade technique established).
2. `IntEfficientR`: `int_efficient` (a = 73) by induction on the `Steps` derivation,
   using the cost corollaries; `int_efficient_time` as executable corollary. **M**
3. Discharge `LevinRunFor`'s `int_efficient_time` hypothesis with 2 → the Rocq Levin
   chain is FULLY self-contained (only the solver's halting fact remains). **S**
