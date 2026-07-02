(** * ProgEncoding -- programs as data: the encoding and its decoder.

    Rocq port (statement-level) of [proofs/agda/ProgEncoding.agda].
    [enc e] (Agda's [⌜ e ⌝]) encodes an [Exp] as a [V] value [(tag . arguments)] with
    unary-numeral tags; [decode] is its structural inverse.  [decode_enc] (Agda's
    [decode-⌜⌝]) is the left-inverse law, on which program-injectivity rests. *)

From Stdlib Require Import Arith.
Require Import UniversalInterp.

(** unary-numeral tags: tag n = a spine of n conses ending in nil. *)
Fixpoint tag (n : nat) : V :=
  match n with
  | 0 => Vnil
  | S n' => Vcons Vnil (tag n')
  end.

(** the encoding  enc e = (tag . arguments). *)
Fixpoint enc (e : Exp) : V :=
  match e with
  | var          => Vcons (tag 0) Vnil
  | knil         => Vcons (tag 1) Vnil
  | kcons a b    => Vcons (tag 2) (Vcons (enc a) (enc b))
  | khd e'       => Vcons (tag 3) (enc e')
  | ktl e'       => Vcons (tag 4) (enc e')
  | kifnil c t e'=> Vcons (tag 5) (Vcons (enc c) (Vcons (enc t) (enc e')))
  | call e'      => Vcons (tag 6) (enc e')
  end.

Notation "⌜ e ⌝" := (enc e) (at level 0).

(** the decoder: structural on the (tag . arguments) shape; invalid codes => None. *)
Fixpoint decode (v : V) : option Exp :=
  match v with
  | Vcons Vnil Vnil => Some var                                        (* tag 0 . nil *)
  | Vcons (Vcons Vnil Vnil) Vnil => Some knil                          (* tag 1 . nil *)
  | Vcons (Vcons Vnil (Vcons Vnil Vnil)) (Vcons va vb) =>              (* tag 2 . (va.vb) *)
      match decode va with
      | None => None
      | Some a => match decode vb with None => None | Some b => Some (kcons a b) end
      end
  | Vcons (Vcons Vnil (Vcons Vnil (Vcons Vnil Vnil))) ve =>            (* tag 3 . ve *)
      match decode ve with None => None | Some e => Some (khd e) end
  | Vcons (Vcons Vnil (Vcons Vnil (Vcons Vnil (Vcons Vnil Vnil)))) ve =>   (* tag 4 . ve *)
      match decode ve with None => None | Some e => Some (ktl e) end
  | Vcons (Vcons Vnil (Vcons Vnil (Vcons Vnil (Vcons Vnil (Vcons Vnil Vnil)))))
          (Vcons vc (Vcons vt ve)) =>                                  (* tag 5 . (vc.(vt.ve)) *)
      match decode vc with None => None | Some c =>
      match decode vt with None => None | Some t =>
      match decode ve with None => None | Some e => Some (kifnil c t e) end end end
  | Vcons (Vcons Vnil (Vcons Vnil (Vcons Vnil (Vcons Vnil (Vcons Vnil (Vcons Vnil Vnil))))))
          ve =>                                                        (* tag 6 . ve *)
      match decode ve with None => None | Some e => Some (call e) end
  | _ => None
  end.

(** the left-inverse law: decode o enc = Some. *)
Lemma decode_enc : forall e, decode (enc e) = Some e.
Proof.
  induction e; simpl.
  - reflexivity.
  - reflexivity.
  - rewrite IHe1, IHe2. reflexivity.
  - rewrite IHe. reflexivity.
  - rewrite IHe. reflexivity.
  - rewrite IHe1, IHe2, IHe3. reflexivity.
  - rewrite IHe. reflexivity.
Qed.
