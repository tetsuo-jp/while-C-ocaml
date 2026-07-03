(** * LevinRunFor -- the concrete sint runner + fully operational, self-contained search.

    Rocq port (statement-level) of [proofs/agda/LevinRunFor.agda] (Levin B3b, final step).
    The interpreter-cost theorem [int_efficient_time] is now DISCHARGED (imported from
    [IntEfficientR], proved via the [SintCostR] dispatch constants), so this module has NO
    remaining hypothesis: [runFor] runs a program via [sint]; [run_hits] discharges the
    [LevinDovetailRun] obligation from [int_efficient_time]
    (+ CostRelation.complete/evalT_count/evalT_mono_le); [levin_optimal_run] composes with
    [LevinComplete.levin_operational] for the fully operational, axiom-free search. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.
Require Import StepCount.
Require Import CostRelation.
Require Import ProgramEnum.
Require Import ProgramDecode.
Require Import LevinBudget.
Require Import LevinOptimal.
Require Import LevinGrounded.
Require Import LevinDovetail.
Require Import LevinDovetailRun.
Require Import LevinComplete.
Require Import IntEfficientR.

(** run program p on x via sint (on the triple enc p . (enc p . x)) with budget b. *)
Definition runFor (b : nat) (p : Exp) (x : V) : option V :=
  match evalT sint b sint (Vcons (enc p) (Vcons (enc p) x)) with
  | Some pr => Some (fst pr)
  | None => None
  end.

(** [int_efficient_time] (Thm 19.1.3 / IntEfficient, a = 73) is now a PROVEN lemma
    imported from [IntEfficientR] -- no longer a hypothesis. *)

Section Solver.
Variables (q : Exp) (x yq : V) (Tq G Tr : nat).
Hypothesis hq : evalT q G q x = Some (yq, Tq).
Variable chk : V -> V -> bool.
Hypothesis chk_yq : chk x yq = true.
Hypothesis Tq_pos : 1 <= Tq.

(** THE run-hits obligation of LevinDovetailRun, discharged. *)
Lemma run_hits_q : forall b, 73 * Tq <= b -> runFor b q x = Some yq.
Proof.
  intros b Hb.
  destruct (int_efficient_time q G x yq Tq hq) as [F [c' [eF Hc']]].
  pose proof (complete sint F sint (Vcons (enc q) (Vcons (enc q) x)) yq c' eF) as D.
  pose proof (evalT_count sint sint (Vcons (enc q) (Vcons (enc q) x)) yq c' D) as Ec.
  assert (Hle : c' <= b) by lia.
  pose proof (evalT_mono_le sint c' b sint (Vcons (enc q) (Vcons (enc q) x)) (yq, c') Hle Ec) as Eb.
  unfold runFor. rewrite Eb. reflexivity.
Qed.

(** same, phrased for enum (index q) = q (what the dovetail addresses). *)
Lemma run_hits_enum : forall b, 73 * Tq <= b -> runFor b (enum (index q)) x = Some yq.
Proof. intros b Hb. rewrite (enum_index q). apply run_hits_q. exact Hb. Qed.

(** the concrete oracle finishes at some stage with the Levin bound. *)
Lemma levin_finishes :
  exists k, doneO runFor chk x k = true /\
    gsum k < aq (search (index q) Tq Tr) * Swork (search (index q) Tq Tr).
Proof.
  apply (finishing runFor chk x (index q) Tq Tr yq chk_yq run_hits_enum Tq_pos).
Qed.

(** FULL end-to-end: the operational dovetail search halts at kop, its real
    (phase-charged) running time gsum kop is Levin-optimal. *)
Lemma levin_optimal_run :
  exists kop, (exists f, find (doneO runFor chk x) f 0 = Some kop) /\
    (time_opt kop = gsum kop) /\
    (gsum kop < aq (search (index q) Tq Tr) * Swork (search (index q) Tq Tr)).
Proof.
  destruct levin_finishes as [k [dk bnd]].
  apply (levin_operational (doneO runFor chk x) (index q) Tq Tr k bnd dk).
Qed.

End Solver.
