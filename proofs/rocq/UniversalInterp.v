(** * UniversalInterp -- values, expressions, and the fuel-indexed interpreter.

    Rocq port (statement-level) of [proofs/agda/UniversalInterp.agda].
    [V] is the binary-tree value domain; [Exp] the one-variable I language; [eval prog n
    e x] runs [e] under program [prog] with fuel [n] (total by structural descent on [n]).
    [mono]/[add_fuel] are fuel-monotonicity.  We use Rocq's [option] for the Agda [Maybe]. *)

From Stdlib Require Import Arith Lia.

(** * values: binary trees (nil / cons). *)
Inductive V : Set :=
| Vnil  : V
| Vcons : V -> V -> V.

Notation "'⟨⟩'" := Vnil.
Infix "∙" := Vcons (at level 60, right associativity).

Definition hd (v : V) : V := match v with Vcons a _ => a | Vnil => Vnil end.
Definition tl (v : V) : V := match v with Vcons _ b => b | Vnil => Vnil end.
Definition ifnil {A : Type} (v : V) (t e : A) : A :=
  match v with Vnil => t | Vcons _ _ => e end.

(** * expressions: the one-variable I language. *)
Inductive Exp : Set :=
| var    : Exp
| knil   : Exp
| kcons  : Exp -> Exp -> Exp
| khd    : Exp -> Exp
| ktl    : Exp -> Exp
| kifnil : Exp -> Exp -> Exp -> Exp
| call   : Exp -> Exp.

(** * the fuel-indexed universal interpreter (parametrised by [prog]). *)
Fixpoint eval (prog : Exp) (n : nat) (e : Exp) (x : V) : option V :=
  match n with
  | 0 => None
  | S n' =>
    match e with
    | var  => Some x
    | knil => Some Vnil
    | kcons a b =>
        match eval prog n' a x with
        | None => None
        | Some va => match eval prog n' b x with
                     | None => None
                     | Some vb => Some (Vcons va vb)
                     end
        end
    | khd e' => match eval prog n' e' x with None => None | Some v => Some (hd v) end
    | ktl e' => match eval prog n' e' x with None => None | Some v => Some (tl v) end
    | kifnil c t e' =>
        match eval prog n' c x with
        | None => None
        | Some vc => ifnil vc (eval prog n' t x) (eval prog n' e' x)
        end
    | call e' =>
        match eval prog n' e' x with
        | None => None
        | Some arg => eval prog n' prog arg
        end
    end
  end.

Definition run (prog : Exp) (n : nat) (x : V) : option V := eval prog n prog x.

(** Keep [eval] folded under [simpl]/[cbn]; unfold one step via the [ev_*] lemmas. *)
Arguments eval : simpl never.

Lemma ev_var  : forall p n x, eval p (S n) var x = Some x.            Proof. reflexivity. Qed.
Lemma ev_knil : forall p n x, eval p (S n) knil x = Some Vnil.        Proof. reflexivity. Qed.
Lemma ev_kcons : forall p n a b x,
  eval p (S n) (kcons a b) x =
  match eval p n a x with
  | None => None
  | Some va => match eval p n b x with None => None | Some vb => Some (Vcons va vb) end
  end.
Proof. reflexivity. Qed.
Lemma ev_khd : forall p n e x,
  eval p (S n) (khd e) x = match eval p n e x with None => None | Some v => Some (hd v) end.
Proof. reflexivity. Qed.
Lemma ev_ktl : forall p n e x,
  eval p (S n) (ktl e) x = match eval p n e x with None => None | Some v => Some (tl v) end.
Proof. reflexivity. Qed.
Lemma ev_kifnil : forall p n c t e x,
  eval p (S n) (kifnil c t e) x =
  match eval p n c x with
  | None => None
  | Some vc => ifnil vc (eval p n t x) (eval p n e x)
  end.
Proof. reflexivity. Qed.
Lemma ev_call : forall p n e x,
  eval p (S n) (call e) x =
  match eval p n e x with None => None | Some arg => eval p n p arg end.
Proof. reflexivity. Qed.

(** * fuel-monotonicity: more fuel never changes an already-defined result. *)
Lemma mono : forall prog n e x v,
  eval prog n e x = Some v -> eval prog (S n) e x = Some v.
Proof.
  intros prog n. induction n as [| n IH]; intros e x v H; [ discriminate |].
  destruct e.
  - rewrite ev_var in H. rewrite ev_var. exact H.
  - rewrite ev_knil in H. rewrite ev_knil. exact H.
  - rewrite ev_kcons in H. rewrite ev_kcons.
    destruct (eval prog n e1 x) as [va|] eqn:Ea; [| discriminate].
    rewrite (IH _ _ _ Ea).
    destruct (eval prog n e2 x) as [vb|] eqn:Eb; [| discriminate].
    rewrite (IH _ _ _ Eb). exact H.
  - rewrite ev_khd in H. rewrite ev_khd.
    destruct (eval prog n e x) as [w|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ Ee). exact H.
  - rewrite ev_ktl in H. rewrite ev_ktl.
    destruct (eval prog n e x) as [w|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ Ee). exact H.
  - rewrite ev_kifnil in H. rewrite ev_kifnil.
    destruct (eval prog n e1 x) as [vc|] eqn:Ec; [| discriminate].
    rewrite (IH _ _ _ Ec). destruct vc as [| a b].
    + destruct (eval prog n e2 x) as [w|] eqn:Et; [| discriminate].
      rewrite (IH _ _ _ Et). exact H.
    + destruct (eval prog n e3 x) as [w|] eqn:Ee'; [| discriminate].
      rewrite (IH _ _ _ Ee'). exact H.
  - rewrite ev_call in H. rewrite ev_call.
    destruct (eval prog n e x) as [arg|] eqn:Ee; [| discriminate].
    rewrite (IH _ _ _ Ee). apply IH. exact H.
Qed.

Lemma add_fuel : forall prog k n e x v,
  eval prog n e x = Some v -> eval prog (k + n) e x = Some v.
Proof.
  intros prog k. induction k as [| k IH]; intros n e x v H; [ exact H |].
  simpl. apply mono. apply IH. exact H.
Qed.

Lemma eval_mono_le : forall prog n n' e x v,
  n <= n' -> eval prog n e x = Some v -> eval prog n' e x = Some v.
Proof.
  intros prog n n' e x v Hle H.
  replace n' with ((n' - n) + n) by lia.
  apply add_fuel. exact H.
Qed.
