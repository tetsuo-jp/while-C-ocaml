(** * StepCount -- a total-step-counting instrumented interpreter.

    Rocq port (statement-level) of [proofs/agda/StepCount.agda] (Levin B1).
    [eval]'s fuel is a DEPTH bound, so it is not the total step count; [evalT n e x =
    Some (v, t)] says [e] evaluates to [v] in [t] unit-cost steps (one per node, summed
    over sub-evaluations).  [evalT_eval] ties it back to [eval] (same value). *)

From Stdlib Require Import Arith.
Require Import UniversalInterp.

(** add [c] steps (and one for the current node) to an optional sub-result. *)
Definition bump (c : nat) (r : option (V * nat)) : option (V * nat) :=
  match r with
  | None => None
  | Some p => Some (fst p, S (c + snd p))
  end.

(** instrumented evaluation: value paired with total unit-cost step count. *)
Fixpoint evalT (prog : Exp) (n : nat) (e : Exp) (x : V) : option (V * nat) :=
  match n with
  | 0 => None
  | S n' =>
    match e with
    | var  => Some (x, 1)
    | knil => Some (Vnil, 1)
    | kcons a b =>
        match evalT prog n' a x with
        | None => None
        | Some ra => match evalT prog n' b x with
                     | None => None
                     | Some rb => Some (Vcons (fst ra) (fst rb), S (snd ra + snd rb))
                     end
        end
    | khd e' => match evalT prog n' e' x with None => None | Some r => Some (hd (fst r), S (snd r)) end
    | ktl e' => match evalT prog n' e' x with None => None | Some r => Some (tl (fst r), S (snd r)) end
    | kifnil c t e' =>
        match evalT prog n' c x with
        | None => None
        | Some rc => ifnil (fst rc) (bump (snd rc) (evalT prog n' t x))
                                    (bump (snd rc) (evalT prog n' e' x))
        end
    | call e' =>
        match evalT prog n' e' x with
        | None => None
        | Some ra => match evalT prog n' prog (fst ra) with
                     | None => None
                     | Some rr => Some (fst rr, S (snd ra + snd rr))
                     end
        end
    end
  end.

Definition timeT (prog : Exp) (n : nat) (x : V) : option (V * nat) := evalT prog n prog x.

Arguments evalT : simpl never.

Lemma evT_var  : forall p n x, evalT p (S n) var x = Some (x, 1).      Proof. reflexivity. Qed.
Lemma evT_knil : forall p n x, evalT p (S n) knil x = Some (Vnil, 1).  Proof. reflexivity. Qed.
Lemma evT_kcons : forall p n a b x,
  evalT p (S n) (kcons a b) x =
  match evalT p n a x with
  | None => None
  | Some ra => match evalT p n b x with
               | None => None
               | Some rb => Some (Vcons (fst ra) (fst rb), S (snd ra + snd rb)) end
  end.
Proof. reflexivity. Qed.
Lemma evT_khd : forall p n e x,
  evalT p (S n) (khd e) x = match evalT p n e x with None => None | Some r => Some (hd (fst r), S (snd r)) end.
Proof. reflexivity. Qed.
Lemma evT_ktl : forall p n e x,
  evalT p (S n) (ktl e) x = match evalT p n e x with None => None | Some r => Some (tl (fst r), S (snd r)) end.
Proof. reflexivity. Qed.
Lemma evT_kifnil : forall p n c t e x,
  evalT p (S n) (kifnil c t e) x =
  match evalT p n c x with
  | None => None
  | Some rc => ifnil (fst rc) (bump (snd rc) (evalT p n t x)) (bump (snd rc) (evalT p n e x))
  end.
Proof. reflexivity. Qed.
Lemma evT_call : forall p n e x,
  evalT p (S n) (call e) x =
  match evalT p n e x with
  | None => None
  | Some ra => match evalT p n p (fst ra) with None => None | Some rr => Some (fst rr, S (snd ra + snd rr)) end
  end.
Proof. reflexivity. Qed.

(** consistency: the instrumented run computes the same value as [eval]. *)
Lemma evalT_eval : forall prog n e x v t,
  evalT prog n e x = Some (v, t) -> eval prog n e x = Some v.
Proof.
  intros prog n; induction n as [| n IH]; intros e x v t H; [ discriminate |].
  destruct e.
  - rewrite evT_var in H. rewrite ev_var. injection H as Hv _. rewrite Hv. reflexivity.
  - rewrite evT_knil in H. rewrite ev_knil. injection H as Hv _. rewrite Hv. reflexivity.
  - rewrite evT_kcons in H. rewrite ev_kcons.
    destruct (evalT prog n e1 x) as [[va ta]|] eqn:Ea; [| discriminate].
    rewrite (IH _ _ _ _ Ea).
    destruct (evalT prog n e2 x) as [[vb tb]|] eqn:Eb; [| discriminate].
    rewrite (IH _ _ _ _ Eb).
    injection H as Hv _. rewrite Hv. reflexivity.
  - rewrite evT_khd in H. rewrite ev_khd.
    destruct (evalT prog n e x) as [[w s]|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ _ Ee). injection H as Hv _. rewrite Hv. reflexivity.
  - rewrite evT_ktl in H. rewrite ev_ktl.
    destruct (evalT prog n e x) as [[w s]|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ _ Ee). injection H as Hv _. rewrite Hv. reflexivity.
  - rewrite evT_kifnil in H. rewrite ev_kifnil.
    destruct (evalT prog n e1 x) as [[vc tc]|] eqn:Ec; [| discriminate].
    rewrite (IH _ _ _ _ Ec). destruct vc as [| a b].
    + destruct (evalT prog n e2 x) as [[w s]|] eqn:Et; simpl in H |- *; [| discriminate].
      rewrite (IH _ _ _ _ Et). injection H as Hv _. rewrite Hv. reflexivity.
    + destruct (evalT prog n e3 x) as [[w s]|] eqn:Ee'; simpl in H |- *; [| discriminate].
      rewrite (IH _ _ _ _ Ee'). injection H as Hv _. rewrite Hv. reflexivity.
  - rewrite evT_call in H. rewrite ev_call.
    destruct (evalT prog n e x) as [[va ta]|] eqn:Ee; simpl in H; [| discriminate].
    rewrite (IH _ _ _ _ Ee).
    destruct (evalT prog n prog va) as [[vr tr]|] eqn:Ep; simpl in H; [| discriminate].
    rewrite (IH _ _ _ _ Ep). injection H as Hv _. rewrite Hv. reflexivity.
Qed.

(** concrete checks: the counter reports the number of nodes evaluated. *)
Example ktl_var_count : evalT knil 2 (ktl var) (Vcons Vnil Vnil) = Some (Vnil, 2).
Proof. reflexivity. Qed.
Example kcons_count : evalT knil 2 (kcons var var) Vnil = Some (Vcons Vnil Vnil, 3).
Proof. reflexivity. Qed.
Example call_count : evalT (ktl var) 3 (call var) (Vcons Vnil Vnil) = Some (Vnil, 4).
Proof. reflexivity. Qed.
