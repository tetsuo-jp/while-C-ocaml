(** * SimF -- self-interpreter correctness, recursive dispatch + call-free sim (SIMF-PLAN).

    The recursive dispatch, CRACKED.  KEY 1 (technique): state each dispatch as a
    SYMMETRIC-fuel equation -- outer fuel = inner + (dispatch cost), inner keeps enough
    CONCRETE steps for the tag-walk guards -- so both sides share the recursive sub-run at
    the SAME fuel and the equation closes by computation.  KEY 2 (performance): prove the
    equations with [vm_compute] (NOT kernel [reflexivity]) -- for symbolic-fuel dispatch
    equations vm_compute reduces the concrete dispatch spine and freezes the symbolic
    sub-runs in ~1s, whereas kernel reflexivity over the giant [sint] with two/three
    sub-runs is exponential (kcons ~100s, kifnil >300s).  The per-constructor lemma then
    lifts the sub-run hypothesis by [eval_mono_le].  [sim] is the call-free G1. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.
Require Import SelfInterpCorrect.

Definition omap (f : V -> V) (o : option V) : option V :=
  match o with Some w => Some (f w) | None => None end.

(** dispatch equations (by vm_compute).  Offsets: khd 12/6, ktl 13/6, kcons 12/7, kifnil 16/8. *)
Lemma disp_khd_eq : forall m p e env,
  eval sint (12 + m) sint (Vcons (enc p) (Vcons (enc (khd e)) env)) =
  omap hd (eval sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))).
Proof. intros. vm_compute. reflexivity. Qed.

Lemma disp_ktl_eq : forall m p e env,
  eval sint (13 + m) sint (Vcons (enc p) (Vcons (enc (ktl e)) env)) =
  omap tl (eval sint (6 + m) sint (Vcons (enc p) (Vcons (enc e) env))).
Proof. intros. vm_compute. reflexivity. Qed.

Lemma disp_kcons_eq : forall m p a b env,
  eval sint (12 + m) sint (Vcons (enc p) (Vcons (enc (kcons a b)) env)) =
  match eval sint (7 + m) sint (Vcons (enc p) (Vcons (enc a) env)) with
  | None => None
  | Some va => match eval sint (7 + m) sint (Vcons (enc p) (Vcons (enc b) env)) with
               | None => None | Some vb => Some (Vcons va vb) end
  end.
Proof. intros. vm_compute. reflexivity. Qed.

Lemma disp_kifnil_eq : forall m p c t e env,
  eval sint (16 + m) sint (Vcons (enc p) (Vcons (enc (kifnil c t e)) env)) =
  match eval sint (8 + m) sint (Vcons (enc p) (Vcons (enc c) env)) with
  | None => None
  | Some vc => ifnil vc (eval sint (8 + m) sint (Vcons (enc p) (Vcons (enc t) env)))
                        (eval sint (8 + m) sint (Vcons (enc p) (Vcons (enc e) env)))
  end.
Proof. intros. vm_compute. reflexivity. Qed.

(** per-constructor dispatch lemmas (lift the sub-run hypothesis by eval_mono_le). *)
Lemma disp_khd : forall F p e env v,
  selfrun F p e env = Some v -> selfrun (12 + F) p (khd e) env = Some (hd v).
Proof.
  intros F p e env v H. unfold selfrun in *.
  assert (H6 : eval sint (6 + F) sint (Vcons (enc p) (Vcons (enc e) env)) = Some v)
    by (apply (eval_mono_le sint F (6 + F)); [ lia | exact H ]).
  rewrite (disp_khd_eq F p e env). rewrite H6. reflexivity.
Qed.

Lemma disp_ktl : forall F p e env v,
  selfrun F p e env = Some v -> selfrun (13 + F) p (ktl e) env = Some (tl v).
Proof.
  intros F p e env v H. unfold selfrun in *.
  assert (H6 : eval sint (6 + F) sint (Vcons (enc p) (Vcons (enc e) env)) = Some v)
    by (apply (eval_mono_le sint F (6 + F)); [ lia | exact H ]).
  rewrite (disp_ktl_eq F p e env). rewrite H6. reflexivity.
Qed.

Lemma disp_kcons : forall F p a b env va vb,
  selfrun F p a env = Some va -> selfrun F p b env = Some vb ->
  selfrun (12 + F) p (kcons a b) env = Some (Vcons va vb).
Proof.
  intros F p a b env va vb Ha Hb. unfold selfrun in *.
  assert (Ha' : eval sint (7 + F) sint (Vcons (enc p) (Vcons (enc a) env)) = Some va)
    by (apply (eval_mono_le sint F (7 + F)); [ lia | exact Ha ]).
  assert (Hb' : eval sint (7 + F) sint (Vcons (enc p) (Vcons (enc b) env)) = Some vb)
    by (apply (eval_mono_le sint F (7 + F)); [ lia | exact Hb ]).
  rewrite (disp_kcons_eq F p a b env). rewrite Ha', Hb'. reflexivity.
Qed.

Lemma disp_kifnil : forall F p c t e env vc v,
  selfrun F p c env = Some vc ->
  ifnil vc (selfrun F p t env) (selfrun F p e env) = Some v ->
  selfrun (16 + F) p (kifnil c t e) env = Some v.
Proof.
  intros F p c t e env vc v Hc Hsel. unfold selfrun in *.
  assert (Hc' : eval sint (8 + F) sint (Vcons (enc p) (Vcons (enc c) env)) = Some vc)
    by (apply (eval_mono_le sint F (8 + F)); [ lia | exact Hc ]).
  rewrite (disp_kifnil_eq F p c t e env). rewrite Hc'.
  destruct vc as [| a b]; simpl in Hsel |- *;
    apply (eval_mono_le sint F (8 + F)); [ lia | exact Hsel | lia | exact Hsel ].
Qed.

(** ------------------------------------------------------------------ *)
(** PHASE 3: the call-free self-interpreter correctness [sim], by induction on [e]. *)

Fixpoint call_free (e : Exp) : Prop :=
  match e with
  | var => True | knil => True
  | khd e' => call_free e' | ktl e' => call_free e'
  | kcons a b => call_free a /\ call_free b
  | kifnil c t e' => call_free c /\ call_free t /\ call_free e'
  | call _ => False
  end.

Lemma selfrun_mono : forall F F' p e env v,
  F <= F' -> selfrun F p e env = Some v -> selfrun F' p e env = Some v.
Proof.
  unfold selfrun. intros F F' p e env v Hle H. apply (eval_mono_le sint F F'); assumption.
Qed.

Lemma sim : forall e, call_free e -> forall p env v G,
  eval p G e env = Some v -> exists F, selfrun F p e env = Some v.
Proof.
  induction e as [ | | a IHa b IHb | e' IHe' | e' IHe'
                 | c IHc t IHt e' IHe' | e' IHe' ];
    intros Hcf p env v G H.
  - destruct G as [| G]; [ discriminate |].
    rewrite ev_var in H. injection H as H'. subst v. exists 20. apply sint_var.
  - destruct G as [| G]; [ discriminate |].
    rewrite ev_knil in H. injection H as H'. subst v. exists 20. apply sint_knil.
  - destruct G as [| G]; [ discriminate |].
    rewrite ev_kcons in H.
    destruct (eval p G a env) as [va|] eqn:Ea; [| discriminate].
    destruct (eval p G b env) as [vb|] eqn:Eb; [| discriminate].
    injection H as H'. subst v. destruct Hcf as [Hca Hcb].
    destruct (IHa Hca p env va G Ea) as [Fa HFa].
    destruct (IHb Hcb p env vb G Eb) as [Fb HFb].
    exists (12 + (Fa + Fb)). apply disp_kcons.
    + apply (selfrun_mono Fa); [ lia | exact HFa ].
    + apply (selfrun_mono Fb); [ lia | exact HFb ].
  - destruct G as [| G]; [ discriminate |].
    rewrite ev_khd in H.
    destruct (eval p G e' env) as [w|] eqn:Ee; [| discriminate].
    injection H as H'. subst v.
    destruct (IHe' Hcf p env w G Ee) as [Fe HFe].
    exists (12 + Fe). apply disp_khd. exact HFe.
  - destruct G as [| G]; [ discriminate |].
    rewrite ev_ktl in H.
    destruct (eval p G e' env) as [w|] eqn:Ee; [| discriminate].
    injection H as H'. subst v.
    destruct (IHe' Hcf p env w G Ee) as [Fe HFe].
    exists (13 + Fe). apply disp_ktl. exact HFe.
  - destruct G as [| G]; [ discriminate |].
    rewrite ev_kifnil in H.
    destruct (eval p G c env) as [vc|] eqn:Ec; [| discriminate].
    destruct Hcf as [Hcc [Hct Hce]].
    destruct (IHc Hcc p env vc G Ec) as [Fc HFc].
    destruct vc as [| x y]; simpl in H.
    + destruct (IHt Hct p env v G H) as [Ft HFt].
      exists (16 + (Fc + Ft)). apply (disp_kifnil _ p c t e' env Vnil v).
      * apply (selfrun_mono Fc); [ lia | exact HFc ].
      * simpl. apply (selfrun_mono Ft); [ lia | exact HFt ].
    + destruct (IHe' Hce p env v G H) as [Fe HFe].
      exists (16 + (Fc + Fe)). apply (disp_kifnil _ p c t e' env (Vcons x y) v).
      * apply (selfrun_mono Fc); [ lia | exact HFc ].
      * simpl. apply (selfrun_mono Fe); [ lia | exact HFe ].
  - destruct Hcf.
Qed.
