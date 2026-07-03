(** * CostRelation -- a fuel-free big-step cost relation, adequate wrt evalT.

    Rocq port (statement-level) of [proofs/agda/CostRelation.agda] (Levin B1.2).
    [Steps prog e env v c] : [e] under [prog] on [env] yields [v] in [c] unit-cost steps.
    Adequacy both directions vs [StepCount.evalT]: [complete] (evalT => Steps),
    [sound] (Steps => some fuel), [evalT_count] (fuel = the exact count suffices).
    A sub-run enters a rule as an opaque premise, dodging fuel bookkeeping. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import StepCount.

Section CostRel.
Variable prog : Exp.

Inductive Steps : Exp -> V -> V -> nat -> Prop :=
| S_var   : forall env, Steps var env env 1
| S_knil  : forall env, Steps knil env Vnil 1
| S_khd   : forall e env v c, Steps e env v c -> Steps (khd e) env (hd v) (S c)
| S_ktl   : forall e env v c, Steps e env v c -> Steps (ktl e) env (tl v) (S c)
| S_kcons : forall a b env va vb ca cb,
    Steps a env va ca -> Steps b env vb cb ->
    Steps (kcons a b) env (Vcons va vb) (S (ca + cb))
| S_then  : forall g t e env vt cg ct,
    Steps g env Vnil cg -> Steps t env vt ct ->
    Steps (kifnil g t e) env vt (S (cg + ct))
| S_else  : forall g t e env vh vl ve cg ce,
    Steps g env (Vcons vh vl) cg -> Steps e env ve ce ->
    Steps (kifnil g t e) env ve (S (cg + ce))
| S_call  : forall e env arg v ca cv,
    Steps e env arg ca -> Steps prog arg v cv ->
    Steps (call e) env v (S (ca + cv)).

(** completeness: a terminating evalT run yields a derivation with equal count. *)
Lemma complete : forall G e env v c,
  evalT prog G e env = Some (v, c) -> Steps e env v c.
Proof.
  induction G as [| G IH]; intros e env v c H; [ discriminate |].
  destruct e.
  - rewrite evT_var in H. injection H as Hv Hc; subst. apply S_var.
  - rewrite evT_knil in H. injection H as Hv Hc; subst. apply S_knil.
  - rewrite evT_kcons in H.
    destruct (evalT prog G e1 env) as [[va ca]|] eqn:Ea; [| discriminate].
    destruct (evalT prog G e2 env) as [[vb cb]|] eqn:Eb; [| discriminate].
    simpl in H. injection H as Hv Hc; subst.
    eapply S_kcons; [ apply (IH _ _ _ _ Ea) | apply (IH _ _ _ _ Eb) ].
  - rewrite evT_khd in H.
    destruct (evalT prog G e env) as [[w s]|] eqn:Ee; [| discriminate].
    simpl in H. injection H as Hv Hc; subst. apply S_khd. apply (IH _ _ _ _ Ee).
  - rewrite evT_ktl in H.
    destruct (evalT prog G e env) as [[w s]|] eqn:Ee; [| discriminate].
    simpl in H. injection H as Hv Hc; subst. apply S_ktl. apply (IH _ _ _ _ Ee).
  - rewrite evT_kifnil in H.
    destruct (evalT prog G e1 env) as [[vc cg]|] eqn:Ec; [| discriminate].
    destruct vc as [| a b]; simpl in H.
    + destruct (evalT prog G e2 env) as [[vt ct]|] eqn:Et; simpl in H; [| discriminate].
      injection H as Hv Hc; subst. eapply S_then; [ apply (IH _ _ _ _ Ec) | apply (IH _ _ _ _ Et) ].
    + destruct (evalT prog G e3 env) as [[ve ce]|] eqn:Ee'; simpl in H; [| discriminate].
      injection H as Hv Hc; subst. eapply S_else; [ apply (IH _ _ _ _ Ec) | apply (IH _ _ _ _ Ee') ].
  - rewrite evT_call in H.
    destruct (evalT prog G e env) as [[va ca]|] eqn:Ee; simpl in H; [| discriminate].
    destruct (evalT prog G prog va) as [[vr cv]|] eqn:Ep; simpl in H; [| discriminate].
    injection H as Hv Hc; subst. eapply S_call; [ apply (IH _ _ _ _ Ee) | apply (IH _ _ _ _ Ep) ].
Qed.

(** evalT fuel-monotonicity (more fuel preserves a defined result). *)
Lemma evalT_mono : forall n e env r,
  evalT prog n e env = Some r -> evalT prog (S n) e env = Some r.
Proof.
  induction n as [| n IH]; intros e env r H; [ discriminate |].
  destruct e.
  - rewrite evT_var in H. rewrite evT_var. exact H.
  - rewrite evT_knil in H. rewrite evT_knil. exact H.
  - rewrite evT_kcons in H. rewrite evT_kcons.
    destruct (evalT prog n e1 env) as [ra|] eqn:Ea; [| discriminate].
    rewrite (IH _ _ _ Ea).
    destruct (evalT prog n e2 env) as [rb|] eqn:Eb; [| discriminate].
    rewrite (IH _ _ _ Eb). exact H.
  - rewrite evT_khd in H. rewrite evT_khd.
    destruct (evalT prog n e env) as [r'|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ Ee). exact H.
  - rewrite evT_ktl in H. rewrite evT_ktl.
    destruct (evalT prog n e env) as [r'|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ Ee). exact H.
  - rewrite evT_kifnil in H. rewrite evT_kifnil.
    destruct (evalT prog n e1 env) as [[vc cg]|] eqn:Ec; [| discriminate].
    rewrite (IH _ _ _ Ec). destruct vc as [| a b].
    + destruct (evalT prog n e2 env) as [r2|] eqn:Et; simpl in H |- *; [| discriminate].
      rewrite (IH _ _ _ Et). exact H.
    + destruct (evalT prog n e3 env) as [r3|] eqn:Ee'; simpl in H |- *; [| discriminate].
      rewrite (IH _ _ _ Ee'). exact H.
  - rewrite evT_call in H. rewrite evT_call.
    destruct (evalT prog n e env) as [[va ca]|] eqn:Ee; simpl in H; [| discriminate].
    rewrite (IH _ _ _ Ee). simpl.
    destruct (evalT prog n prog va) as [rr|] eqn:Ep; [| discriminate].
    rewrite (IH _ _ _ Ep). exact H.
Qed.

Lemma evalT_mono_add : forall k n e env r,
  evalT prog n e env = Some r -> evalT prog (k + n) e env = Some r.
Proof.
  induction k as [| k IH]; intros n e env r H; [ exact H |].
  simpl. apply evalT_mono. apply IH. exact H.
Qed.

Lemma evalT_mono_le : forall n n' e env r,
  n <= n' -> evalT prog n e env = Some r -> evalT prog n' e env = Some r.
Proof.
  intros n n' e env r Hle H.
  replace n' with ((n' - n) + n) by lia.
  apply evalT_mono_add. exact H.
Qed.

(** soundness: every derivation is realised by some evalT run (same value+count). *)
Lemma sound : forall e env v c, Steps e env v c -> exists G, evalT prog G e env = Some (v, c).
Proof.
  intros e env v c D. induction D.
  - exists 1. reflexivity.
  - exists 1. reflexivity.
  - destruct IHD as [G HG]. exists (S G). rewrite evT_khd, HG. reflexivity.
  - destruct IHD as [G HG]. exists (S G). rewrite evT_ktl, HG. reflexivity.
  - destruct IHD1 as [Ga Ha]. destruct IHD2 as [Gb Hb]. exists (S (Ga + Gb)).
    rewrite evT_kcons.
    rewrite (evalT_mono_le Ga (Ga + Gb) a env (va, ca) ltac:(lia) Ha).
    rewrite (evalT_mono_le Gb (Ga + Gb) b env (vb, cb) ltac:(lia) Hb). reflexivity.
  - destruct IHD1 as [Gg Hg]. destruct IHD2 as [Gt Ht]. exists (S (Gg + Gt)).
    rewrite evT_kifnil.
    rewrite (evalT_mono_le Gg (Gg + Gt) g env (Vnil, cg) ltac:(lia) Hg). simpl.
    rewrite (evalT_mono_le Gt (Gg + Gt) t env (vt, ct) ltac:(lia) Ht). simpl. reflexivity.
  - destruct IHD1 as [Gg Hg]. destruct IHD2 as [Ge He]. exists (S (Gg + Ge)).
    rewrite evT_kifnil.
    rewrite (evalT_mono_le Gg (Gg + Ge) g env (Vcons vh vl, cg) ltac:(lia) Hg). simpl.
    rewrite (evalT_mono_le Ge (Gg + Ge) e env (ve, ce) ltac:(lia) He). simpl. reflexivity.
  - destruct IHD1 as [Ge He]. destruct IHD2 as [Gp Hp]. exists (S (Ge + Gp)).
    rewrite evT_call.
    rewrite (evalT_mono_le Ge (Ge + Gp) e env (arg, ca) ltac:(lia) He). simpl.
    rewrite (evalT_mono_le Gp (Ge + Gp) prog arg (v, cv) ltac:(lia) Hp). simpl. reflexivity.
Qed.

(** canonical fuel: fuel = the exact step count already suffices. *)
Lemma evalT_count : forall e env v c, Steps e env v c -> evalT prog c e env = Some (v, c).
Proof.
  intros e env v c D. induction D.
  - reflexivity.
  - reflexivity.
  - rewrite evT_khd, IHD. reflexivity.
  - rewrite evT_ktl, IHD. reflexivity.
  - rewrite evT_kcons.
    rewrite (evalT_mono_le ca (ca + cb) a env (va, ca) ltac:(lia) IHD1).
    rewrite (evalT_mono_le cb (ca + cb) b env (vb, cb) ltac:(lia) IHD2). reflexivity.
  - rewrite evT_kifnil.
    rewrite (evalT_mono_le cg (cg + ct) g env (Vnil, cg) ltac:(lia) IHD1). simpl.
    rewrite (evalT_mono_le ct (cg + ct) t env (vt, ct) ltac:(lia) IHD2). simpl. reflexivity.
  - rewrite evT_kifnil.
    rewrite (evalT_mono_le cg (cg + ce) g env (Vcons vh vl, cg) ltac:(lia) IHD1). simpl.
    rewrite (evalT_mono_le ce (cg + ce) e env (ve, ce) ltac:(lia) IHD2). simpl. reflexivity.
  - rewrite evT_call.
    rewrite (evalT_mono_le ca (ca + cv) e env (arg, ca) ltac:(lia) IHD1). simpl.
    rewrite (evalT_mono_le cv (ca + cv) prog arg (v, cv) ltac:(lia) IHD2). simpl. reflexivity.
Qed.

End CostRel.
