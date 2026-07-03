(** * SimFFull -- FULL self-interpreter correctness [simF], by induction on
    the OBJECT fuel [G] (covers [call], unlike SimF.sim's structural induction on [e]).
    The call-dispatch lemma is a Section Hypothesis ([disp_call], cost [Kcall]);
    its concrete offsets are being established separately. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp ProgEncoding SelfInterp SelfInterpCorrect SimF DispCall.

Section FullSimF.
Variable Kcall : nat.
Hypothesis disp_call : forall F1 F2 p e env arg v,
  selfrun F1 p e env = Some arg ->
  selfrun F2 p p arg = Some v ->
  selfrun (Kcall + (F1 + F2)) p (call e) env = Some v.

Lemma simF : forall G e p env v,
  eval p G e env = Some v -> exists F, selfrun F p e env = Some v.
Proof.
  induction G as [| G IHG].
  - intros e p env v H. discriminate.
  - intros e p env v H.
    destruct e as [ | | a b | e' | e' | c t e' | e' ].
    + (* var *)
      rewrite ev_var in H. injection H as H'. subst v. exists 20. apply sint_var.
    + (* knil *)
      rewrite ev_knil in H. injection H as H'. subst v. exists 20. apply sint_knil.
    + (* kcons *)
      rewrite ev_kcons in H.
      destruct (eval p G a env) as [va|] eqn:Ea; [| discriminate].
      destruct (eval p G b env) as [vb|] eqn:Eb; [| discriminate].
      injection H as H'. subst v.
      destruct (IHG a p env va Ea) as [Fa HFa].
      destruct (IHG b p env vb Eb) as [Fb HFb].
      exists (12 + (Fa + Fb)). apply disp_kcons.
      * apply (selfrun_mono Fa); [ lia | exact HFa ].
      * apply (selfrun_mono Fb); [ lia | exact HFb ].
    + (* khd *)
      rewrite ev_khd in H.
      destruct (eval p G e' env) as [w|] eqn:Ee; [| discriminate].
      injection H as H'. subst v.
      destruct (IHG e' p env w Ee) as [Fe HFe].
      exists (12 + Fe). apply disp_khd. exact HFe.
    + (* ktl *)
      rewrite ev_ktl in H.
      destruct (eval p G e' env) as [w|] eqn:Ee; [| discriminate].
      injection H as H'. subst v.
      destruct (IHG e' p env w Ee) as [Fe HFe].
      exists (13 + Fe). apply disp_ktl. exact HFe.
    + (* kifnil *)
      rewrite ev_kifnil in H.
      destruct (eval p G c env) as [vc|] eqn:Ec; [| discriminate].
      destruct (IHG c p env vc Ec) as [Fc HFc].
      destruct vc as [| x y]; simpl in H.
      * destruct (IHG t p env v H) as [Ft HFt].
        exists (16 + (Fc + Ft)). apply (disp_kifnil _ p c t e' env Vnil v).
        -- apply (selfrun_mono Fc); [ lia | exact HFc ].
        -- simpl. apply (selfrun_mono Ft); [ lia | exact HFt ].
      * destruct (IHG e' p env v H) as [Fe HFe].
        exists (16 + (Fc + Fe)). apply (disp_kifnil _ p c t e' env (Vcons x y) v).
        -- apply (selfrun_mono Fc); [ lia | exact HFc ].
        -- simpl. apply (selfrun_mono Fe); [ lia | exact HFe ].
    + (* call *)
      rewrite ev_call in H.
      destruct (eval p G e' env) as [arg|] eqn:Ee; [| discriminate].
      destruct (IHG e' p env arg Ee) as [F1 HF1].
      destruct (IHG p p arg v H) as [F2 HF2].
      exists (Kcall + (F1 + F2)).
      apply (disp_call F1 F2 p e' env arg v); assumption.
Qed.

End FullSimF.

(** ------------------------------------------------------------------ *)
(** THE UNCONDITIONAL FULL THEOREM (G1 in Rocq): instantiate the section
    hypothesis with the proven [DispCall.disp_call] at Kcall = 20.  The
    self-interpreter matches the direct interpreter for EVERY program
    (including [call]/recursion) and input, whenever the latter terminates. *)

Lemma simF_full : forall G e p env v,
  eval p G e env = Some v -> exists F, selfrun F p e env = Some v.
Proof. exact (simF 20 disp_call). Qed.

(** universal-program form: run p on input x via the self-interpreter. *)
Corollary selfrun_correct : forall G p x v,
  eval p G p x = Some v -> exists F, selfrun F p p x = Some v.
Proof. intros G p x v H. apply (simF_full G p p x v H). Qed.
