(** * ProgramEnum -- programs are countable: an injective Goedel numbering.

    Rocq port (statement-level) of [proofs/agda/ProgramEnum.agda].
    Route [Exp --enc--> V --vnat--> nat], both legs injective: [enc] via [decode_enc]
    (left inverse), [vnat] via the Cantor [pair] on the two subtrees.  [index_injective]:
    distinct programs get distinct numbers, so each solver has a unique index. *)

From Stdlib Require Import Arith.
Require Import NatPairing.
Require Import UniversalInterp.
Require Import ProgEncoding.

(** number the binary tree V via the Cantor pairing on its subtrees. *)
Fixpoint vnat (v : V) : nat :=
  match v with
  | Vnil => 0
  | Vcons a b => S (pair (vnat a) (vnat b))
  end.

Lemma vnat_inj : forall u v, vnat u = vnat v -> u = v.
Proof.
  intros u; induction u as [| a IHa b IHb];
    intros v H; destruct v as [| c d]; simpl in H.
  - reflexivity.
  - discriminate.
  - discriminate.
  - injection H as H'. apply pair_inj in H'. destruct H' as [Ha Hb].
    f_equal; [ apply IHa; exact Ha | apply IHb; exact Hb ].
Qed.

(** enc is injective (decode is its left inverse). *)
Lemma enc_inj : forall p q, enc p = enc q -> p = q.
Proof.
  intros p q H.
  assert (decode (enc p) = decode (enc q)) as H2 by (rewrite H; reflexivity).
  rewrite !decode_enc in H2. injection H2 as H2. exact H2.
Qed.

Definition index (e : Exp) : nat := vnat (enc e).

Lemma index_injective : forall p q, index p = index q -> p = q.
Proof.
  intros p q H. apply enc_inj, vnat_inj. exact H.
Qed.

(** concrete: var is numbered 1  (enc var = (nil.nil), vnat = S (pair 0 0) = 1). *)
Example index_var : index var = 1. Proof. reflexivity. Qed.
