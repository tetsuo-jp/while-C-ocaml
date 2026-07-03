(** * IntEfficientR -- the self-interpreter has LINEAR overhead (a = 73).

    Rocq port of [proofs/agda/IntEfficient.agda] (Jones Thm 19.1.3, Levin B1).
    By induction on the OBJECT cost derivation [Steps p e env v c] (CostRelation),
    the self-interpreter reproduces the value in at most [73 * c] steps:
    [evalT sint F sint (enc p . (enc e . env)) = Some (v, c')] with [c' <= 73 * c].
    Each case feeds the IH sub-results to the matching cost-dispatch corollary
    (SintCostR: var 8 / knil 12 / khd 39 / ktl 48 / kcons 45 / kifnil 73 / call 64,
    all = the Agda twin's constants; a = 73 = max) and closes the node inequality
    [K + sum(73*ci) <= 73*(1 + sum ci)] by [lia].

    [int_efficient_time] is the executable corollary -- EXACTLY the hypothesis
    [LevinRunFor] assumes, so importing this module makes the Rocq Levin chain
    fully self-contained. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.
Require Import StepCount.
Require Import CostRelation.
Require Import SintCostR.

(** THE LINEAR-OVERHEAD THEOREM (relational form), by induction on the derivation. *)
Lemma int_efficient : forall p e env v c,
  Steps p e env v c ->
  exists F c',
    evalT sint F sint (Vcons (enc p) (Vcons (enc e) env)) = Some (v, c') /\ c' <= 73 * c.
Proof.
  intros p e env v c D. induction D.
  - (* var: 8 <= 73 *)
    exists 20, 8. split; [ apply cost_var | lia ].
  - (* knil: 12 <= 73 *)
    exists 20, 12. split; [ apply cost_knil | lia ].
  - (* khd: 39 + c' <= 73 * S c *)
    destruct IHD as [F [c' [HF Hle]]].
    exists (12 + F), (39 + c'). split; [| lia ].
    apply (cost_khd F). apply (evalT_mono_le sint F (6 + F)); [ lia | exact HF ].
  - (* ktl: 48 + c' <= 73 * S c *)
    destruct IHD as [F [c' [HF Hle]]].
    exists (13 + F), (48 + c'). split; [| lia ].
    apply (cost_ktl F). apply (evalT_mono_le sint F (6 + F)); [ lia | exact HF ].
  - (* kcons: 45 + (ca' + cb') <= 73 * S (ca + cb) *)
    destruct IHD1 as [Fa [ca' [Ha Hla]]]. destruct IHD2 as [Fb [cb' [Hb Hlb]]].
    exists (12 + (Fa + Fb)), (45 + (ca' + cb')). split; [| lia ].
    apply (cost_kcons (Fa + Fb)).
    + apply (evalT_mono_le sint Fa (7 + (Fa + Fb))); [ lia | exact Ha ].
    + apply (evalT_mono_le sint Fb (7 + (Fa + Fb))); [ lia | exact Hb ].
  - (* kifnil/then: 73 + (cg' + ct') <= 73 * S (cg + ct) *)
    destruct IHD1 as [Fg [cg' [Hg Hlg]]]. destruct IHD2 as [Ft [ct' [Ht Hlt]]].
    exists (16 + (Fg + Ft)), (73 + (cg' + ct')). split; [| lia ].
    apply (cost_kifnil_then (Fg + Ft)).
    + apply (evalT_mono_le sint Fg (8 + (Fg + Ft))); [ lia | exact Hg ].
    + apply (evalT_mono_le sint Ft (8 + (Fg + Ft))); [ lia | exact Ht ].
  - (* kifnil/else *)
    destruct IHD1 as [Fg [cg' [Hg Hlg]]]. destruct IHD2 as [Fe [ce' [He Hle]]].
    exists (16 + (Fg + Fe)), (73 + (cg' + ce')). split; [| lia ].
    eapply cost_kifnil_else.
    + apply (evalT_mono_le sint Fg (8 + (Fg + Fe))); [ lia | exact Hg ].
    + apply (evalT_mono_le sint Fe (8 + (Fg + Fe))); [ lia | exact He ].
  - (* call: 64 + (ca' + cv') <= 73 * S (ca + cv) *)
    destruct IHD1 as [Fe [ca' [He Hla]]]. destruct IHD2 as [Fp [cv' [Hp Hlv]]].
    exists (20 + (Fe + Fp)), (64 + (ca' + cv')). split; [| lia ].
    eapply cost_call.
    + apply (evalT_mono_le sint Fe (10 + (Fe + Fp))); [ lia | exact He ].
    + apply (evalT_mono_le sint Fp (13 + (Fe + Fp))); [ lia | exact Hp ].
Qed.

(** Executable corollary (Thm 19.1.3): if p on d halts in c steps, the self-interpreter
    fed (enc p . (enc p . d)) halts in c' <= 73*c steps.  This is EXACTLY the
    [int_efficient_time] hypothesis of LevinRunFor. *)
Lemma int_efficient_time : forall p G d v c,
  evalT p G p d = Some (v, c) ->
  exists F c',
    evalT sint F sint (Vcons (enc p) (Vcons (enc p) d)) = Some (v, c') /\ c' <= 73 * c.
Proof.
  intros p G d v c H. exact (int_efficient p p d v c (complete p G p d v c H)).
Qed.
