(** * SelfInterpCorrect -- self-interpreter correctness (partial: base dispatch).

    Toward the Rocq port of [proofs/agda/SelfInterpCorrect.agda] (G1, [simF]):
      forall p e env v, eval p G e env = Some v -> exists F, selfrun F p e env = Some v.

    STATUS: the BASE dispatch lemmas (var/knil, no sub-run) are proved here by kernel
    lazy conversion ([reflexivity]) with symbolic [p]/[env].  The RECURSIVE dispatch
    (khd/ktl/kcons/kifnil/call) is NOT yet ported -- see the obstacle note below.

    OBSTACLE (recursive dispatch).  With the per-node (depth) fuel model, running [sint]
    on [enc (khd e)] must (a) walk the concrete tag-3 spine -- whose guards
    [ktl (ktl (ktl tagE))] reach depth ~7 and so need ~7 CONCRETE fuel -- and then (b)
    reach the recursive sub-run [selfrun F p e env], which must stay FROZEN (fuel = the
    bare symbolic [F]) to match the induction hypothesis.  These conflict: enough concrete
    fuel for the guards (outer = K+F with K >= ~10) leaves the recursive call at fuel
    [S^(K-6) F], which reduces further -- into interpreting the symbolic [e] -- and gets
    stuck deep, so no single [K] makes [reflexivity]/[cbn] close the dispatch (scanned
    K=4..12).  A faithful port needs manual [ev_*]-stepping with a separate guard
    fuel-sufficiency lemma (freezing the recursive call at bare [F]) -- a dedicated
    multi-session effort, as in the Agda original (constants by "fuel grid search").

    Confirmed (three formulations, all fail by [reflexivity]/[cbn], scanning offsets):
    (i) [selfrun (K+F) p (khd e) env = Some (hd v)] given the sub-run (K=4..12);
    (ii) the reduction equation [= match selfrun F ... with ...] (K=4..12);
    (iii) the SYMMETRIC-fuel equation [selfrun (K+m) p (khd e) env = omap hd (selfrun m ...)]
          (K=4..8).  The guards partially reduce into the symbolic fuel tail on the outer
          side but not on the inner, so no offset makes the two sides definitionally equal.
    The way forward is manual [ev_*] stepping (never [reflexivity] on the recursive call)
    + a guard-eval lemma, NOT a computational tactic. *)

From Stdlib Require Import Arith.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import SelfInterp.

(** [selfrun F p e env] = run [sint] (with itself as global program) on the triple
    [(enc p . (enc e . env))] for [F] steps -- i.e. interpret [e] under [p] on [env]. *)
Definition selfrun (F : nat) (p e : Exp) (env : V) : option V :=
  eval sint F sint (Vcons (enc p) (Vcons (enc e) env)).

(** BASE dispatch (symbolic p, env), by kernel lazy conversion: *)

(** var  =>  env *)
Lemma sint_var : forall p env, selfrun 20 p var env = Some env.
Proof. intros p env. reflexivity. Qed.

(** knil  =>  nil *)
Lemma sint_knil : forall p env, selfrun 20 p knil env = Some Vnil.
Proof. intros p env. reflexivity. Qed.

(** existential (simF-shaped) forms of the base cases. *)
Lemma simF_var : forall p env, exists F, selfrun F p var env = Some env.
Proof. intros p env. exists 20. apply sint_var. Qed.

Lemma simF_knil : forall p env, exists F, selfrun F p knil env = Some Vnil.
Proof. intros p env. exists 20. apply sint_knil. Qed.

(** ------------------------------------------------------------------ *)
(** PHASE 1 of the recursive-dispatch plan (see SIMF-PLAN.md): guard & accessor
    evaluation, all by kernel lazy [reflexivity] (symbolic p,e,env pass through).
    These are the fully-concrete building blocks the Phase-2 dispatch lemmas rewrite with. *)

Definition triple (p e : Exp) (env : V) : V := Vcons (enc p) (Vcons (enc e) env).

(** accessors: P => enc p, R => env (the interpreted program's input). *)
Lemma acc_P : forall p e env, eval sint 6 P (triple p e env) = Some (enc p).
Proof. intros. reflexivity. Qed.

Lemma acc_R : forall p e env, eval sint 6 R (triple p e env) = Some env.
Proof. intros. reflexivity. Qed.

(** for a khd-input, the sub-request builds exactly the sub-triple (enc p . (enc e . env)). *)
Lemma acc_subtriple_khd : forall p e env,
  eval sint 12 (kcons P (kcons argsE R)) (triple p (khd e) env)
  = Some (Vcons (enc p) (Vcons (enc e) env)).
Proof. intros. reflexivity. Qed.

(** the tag-3 walk on a khd-input: the deepest guard reduces to nil (selects the khd arm). *)
Lemma guard_khd_deep : forall p e env,
  eval sint 12 (ktl (ktl (ktl (khd (khd (ktl var)))))) (triple p (khd e) env) = Some Vnil.
Proof. intros. reflexivity. Qed.
