(** * LevinDovetailRun -- the operational phase oracle over an abstract runner.

    Rocq port (statement-level) of [proofs/agda/LevinDovetailRun.agda] (Levin B3b, made
    small).  The Jones STAGE-model oracle [doneO] (stage i runs programs 0..i, program m
    for [pow2 (i-m)] steps of the abstract [run]); [done_sound], [done_intro], and
    [finishing] (given [run_hits], stage k=j+d fires with [gsum k < aq*Swork]) are proved
    over an abstract [run] + the single hypothesis [run_hits]. *)

From Stdlib Require Import Arith Lia Bool.
Require Import LevinBudget.
Require Import LevinOptimal.
Require Import LevinGrounded.
Require Import UniversalInterp.
Require Import ProgramDecode.

Section Oracle.
Variable run : nat -> Exp -> V -> option V.
Variable chk : V -> V -> bool.
Variable x : V.

Definition hitOn (r : option V) : bool := match r with Some y => chk x y | None => false end.
Definition hitb (i m : nat) : bool := hitOn (run (pow2 (i - m)) (enum m) x).

Fixpoint anyHit (i b : nat) : bool :=
  match b with
  | 0 => hitb i 0
  | S m => orb (hitb i (S m)) (anyHit i m)
  end.

Definition doneO (i : nat) : bool := anyHit i i.

(** soundness. *)
Lemma hitb_sound : forall i m, hitb i m = true -> exists y, chk x y = true.
Proof.
  intros i m H. unfold hitb, hitOn in H.
  destruct (run (pow2 (i - m)) (enum m) x) as [y|]; [ exists y; exact H | discriminate ].
Qed.

Lemma anyHit_sound : forall i b, anyHit i b = true -> exists m, m <= b /\ hitb i m = true.
Proof.
  induction b as [| b IH]; intros H.
  - exists 0. split; [ apply le_n | exact H ].
  - simpl in H. apply orb_true_iff in H. destruct H as [H | H].
    + exists (S b). split; [ apply le_n | exact H ].
    + destruct (IH H) as [m [Hle Hh]]. exists m. split; [ lia | exact Hh ].
Qed.

Lemma done_sound : forall i, doneO i = true -> exists y, chk x y = true.
Proof.
  intros i H. unfold doneO in H.
  destruct (anyHit_sound i i H) as [m [_ Hh]]. apply (hitb_sound i m Hh).
Qed.

(** introduction: a hitting program in the batch makes the phase true. *)
Lemma anyHit_intro : forall i b m, m <= b -> hitb i m = true -> anyHit i b = true.
Proof.
  induction b as [| b IH]; intros m Hle Hh.
  - assert (m = 0) by lia. subst. exact Hh.
  - simpl. apply orb_true_iff. destruct (Nat.eq_dec m (S b)) as [He | Hne].
    + left. subst. exact Hh.
    + right. apply (IH m); [ lia | exact Hh ].
Qed.

Lemma done_intro : forall i m, m <= i -> hitb i m = true -> doneO i = true.
Proof. intros i m Hle Hh. unfold doneO. apply (anyHit_intro i i m); assumption. Qed.

(** FINISHING: the solver q = enum j (reaching yq within budget >= 73*Tq) makes some
    stage k = j+d succeed, with gsum k < aq*(Tq+Tr). *)
Section Finishing.
Variables (j Tq Tr : nat) (yq : V).
Hypothesis chk_yq   : chk x yq = true.
Hypothesis run_hits : forall b, 73 * Tq <= b -> run b (enum j) x = Some yq.
Hypothesis Tq_pos   : 1 <= Tq.

Lemma finishing :
  exists k, doneO k = true /\ gsum k < aq (search j Tq Tr) * Swork (search j Tq Tr).
Proof.
  assert (H73 : 1 <= 73 * Tq) by lia.
  destruct (overshoot' (73 * Tq) H73) as [d [Hlo Hhi]].
  exists (j + d). split.
  - apply (done_intro (j + d) j); [ lia |].
    unfold hitb. replace (j + d - j) with d by lia.
    rewrite (run_hits (pow2 d) Hlo). unfold hitOn. exact chk_yq.
  - pose proof (gsum_lt_2last (j + d)) as Hg.
    assert (Hpow : pow2 (S (j + d)) = pow2 j * pow2 d + pow2 j * pow2 d).
    { simpl. rewrite (pow2_add j d). reflexivity. }
    rewrite Hpow in Hg.
    assert (Haq : aq (search j Tq Tr) =
                  (73 * pow2 j + 73 * pow2 j) + (73 * pow2 j + 73 * pow2 j)) by reflexivity.
    assert (Hsw : Swork (search j Tq Tr) = Tq + Tr) by reflexivity.
    rewrite Haq, Hsw.
    set (P := pow2 j) in *. set (D := pow2 d) in *. nia.
Qed.
End Finishing.
End Oracle.
