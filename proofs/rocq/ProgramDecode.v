(** * ProgramDecode -- the total surjective decoder enum : nat -> Exp.

    Rocq port (statement-level) of [proofs/agda/ProgramDecode.agda].
    Inverts the three legs of [index = vnat o enc]: [unpair] (NatPairing), [vunnat]
    (a fuel-driven inverse of [vnat]; fuel = the number itself suffices), and [decode].
    [enum_index] : [enum (index p) = p], so [index] is a section and [enum] a retraction. *)

From Stdlib Require Import Arith Lia.
Require Import NatPairing.
Require Import UniversalInterp.
Require Import ProgEncoding.
Require Import ProgramEnum.

(** invert vnat.  Fuel = the number itself; [vnat v] steps always suffice. *)
Fixpoint vunnat' (f n : nat) : V :=
  match f with
  | 0 => Vnil
  | S f' =>
    match n with
    | 0 => Vnil
    | S n' => let p := unpair n' in Vcons (vunnat' f' (fst p)) (vunnat' f' (snd p))
    end
  end.

Definition vunnat (n : nat) : V := vunnat' n n.

Lemma vunnat'_vnat : forall f v, vnat v <= f -> vunnat' f (vnat v) = v.
Proof.
  induction f as [| f IHf]; intros v Hle.
  - destruct v as [| a b]; [ reflexivity | simpl in Hle; lia ].
  - destruct v as [| a b]; [ reflexivity |].
    simpl. rewrite unpair_pair. simpl.
    pose proof (pair_fst_le (vnat a) (vnat b)) as Hf.
    pose proof (pair_snd_le (vnat a) (vnat b)) as Hs.
    simpl in Hle.
    rewrite (IHf a) by lia. rewrite (IHf b) by lia. reflexivity.
Qed.

Lemma vunnat_vnat : forall v, vunnat (vnat v) = v.
Proof.
  intro v. unfold vunnat. apply vunnat'_vnat. apply le_n.
Qed.

(** the total surjective decoder. *)
Definition enum (n : nat) : Exp :=
  match decode (vunnat n) with
  | Some e => e
  | None => var
  end.

Lemma enum_index : forall p, enum (index p) = p.
Proof.
  intro p. unfold enum, index.
  rewrite vunnat_vnat, decode_enc. reflexivity.
Qed.

(** concrete checks. *)
Example enum_index_var : enum (index var) = var. Proof. reflexivity. Qed.
Example enum_index_khd : enum (index (khd (ktl var))) = khd (ktl var). Proof. reflexivity. Qed.
