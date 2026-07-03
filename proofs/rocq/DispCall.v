(** * DispCall -- fuel dispatch equation for sint's [call] case.

    Offsets (A,B,C) = (20,10,13):
      A = 20 : outer fuel on [eval sint (A+m) sint (p, enc (call e), env)];
      B = 10 : fuel at which the argument [e] is interpreted (A-10: 6 kifnil
               dispatch levels + call node + two kcons env-building levels +
               inner call node);
      C = 13 : fuel at which the apply [p on arg] runs (A-7: 6 dispatch levels
               + the outer call node).

    NOTE the RHS shape: the plain single-match form
      [match eval .. (10+m) .. with Some arg => eval .. (13+m) .. (p,p,arg)]
    is propositionally true but NOT provable by [vm_compute; reflexivity] --
    the LHS normal form keeps the apply-environment construction inside the
    nested option-matches produced by [kcons]/[call] evaluation, so the two
    normal forms differ syntactically.  We state the equation in the exact
    shape vm_compute produces (three nested matches); the clean composition
    lemma [disp_call] below recovers the usable form. *)

From Stdlib Require Import Arith Lia.
Require Import UniversalInterp ProgEncoding SelfInterp SelfInterpCorrect.

(** Dispatch equation, offsets (A,B,C) = (20,10,13), shape-faithful RHS. *)
Lemma disp_call_eq : forall m p e env,
  eval sint (20 + m) sint (Vcons (enc p) (Vcons (enc (call e)) env)) =
  match
    (match
       (match eval sint (10 + m) sint (Vcons (enc p) (Vcons (enc e) env)) with
        | None => None
        | Some vb' => Some (Vcons (enc p) vb')
        end)
     with
     | None => None
     | Some vb => Some (Vcons (enc p) vb)
     end)
  with
  | None => None
  | Some argenv => eval sint (13 + m) sint argenv
  end.
Proof. intros. vm_compute. reflexivity. Qed.

(** Composition: if [e] self-interprets to [arg] with fuel F1 and applying
    [p] to [arg] self-interprets to [v] with fuel F2, then [call e]
    self-interprets to [v] with fuel 20 + (F1 + F2). *)
Lemma disp_call : forall F1 F2 p e env arg v,
  selfrun F1 p e env = Some arg ->
  selfrun F2 p p arg = Some v ->
  selfrun (20 + (F1 + F2)) p (call e) env = Some v.
Proof.
  intros F1 F2 p e env arg v H1 H2.
  unfold selfrun in *.
  assert (H1' : eval sint (10 + (F1 + F2)) sint
                  (Vcons (enc p) (Vcons (enc e) env)) = Some arg).
  { eapply eval_mono_le; [| exact H1]. lia. }
  assert (H2' : eval sint (13 + (F1 + F2)) sint
                  (Vcons (enc p) (Vcons (enc p) arg)) = Some v).
  { eapply eval_mono_le; [| exact H2]. lia. }
  rewrite disp_call_eq, H1'. exact H2'.
Qed.
