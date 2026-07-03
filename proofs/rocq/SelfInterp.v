(** * SelfInterp -- the concrete self-interpreter term sint : Exp.

    Rocq port (statement-level) of [proofs/agda/SelfInterp.agda].
    [sint] interprets an encoded program over the whole object language, run by the
    universal interpreter with itself as the global program; its input is the triple
    [(enc p . (enc e . env))].  A 7-way dispatch on the tag (walked by [ktl]).  Verified
    on concrete programs exercising every constructor incl. recursion (via [call]). *)

From Stdlib Require Import Arith.
Require Import UniversalInterp.
Require Import ProgEncoding.

(** accessors into the triple  var = (enc p . (enc e . env)). *)
Definition P     : Exp := khd var.              (* enc p *)
Definition E     : Exp := khd (ktl var).        (* enc e *)
Definition R     : Exp := ktl (ktl var).        (* env   *)
Definition tagE  : Exp := khd E.                (* enc e's tag (unary numeral) *)
Definition argsE : Exp := ktl E.                (* enc e's argument(s) *)

(** a sub-request: interpret code subE with environment envE. *)
Definition ireq (subE envE : Exp) : Exp := call (kcons P (kcons subE envE)).

(** the self-interpreter: a 7-way dispatch on the tag. *)
Definition sint : Exp :=
  kifnil tagE
    R                                                        (* 0  var   => env *)
    (kifnil (ktl tagE)
      knil                                                   (* 1  knil  => nil *)
      (kifnil (ktl (ktl tagE))
        (kcons (ireq (khd argsE) R) (ireq (ktl argsE) R))    (* 2  kcons => (a . b) *)
        (kifnil (ktl (ktl (ktl tagE)))
          (khd (ireq argsE R))                               (* 3  khd   => hd e *)
          (kifnil (ktl (ktl (ktl (ktl tagE))))
            (ktl (ireq argsE R))                             (* 4  ktl   => tl e *)
            (kifnil (ktl (ktl (ktl (ktl (ktl tagE)))))
              (kifnil (ireq (khd argsE) R)                   (* 5  kifnil => ifnil c *)
                (ireq (khd (ktl argsE)) R)                   (*              t *)
                (ireq (ktl (ktl argsE)) R))                  (*              e *)
              (ireq P (ireq argsE R))))))).                  (* 6  call  => p with env e *)

(** verified on concrete programs (run with itself as the global program):
    selfrun p inp = run sint F (enc p . (enc p . inp)). *)
Definition d : V := Vcons Vnil (Vcons Vnil Vnil).   (* a 2-list [nil, nil] *)

Example self_var  : run sint 15 (Vcons (enc var) (Vcons (enc var) d)) = Some d.
Proof. reflexivity. Qed.

Example self_ktl  : run sint 20 (Vcons (enc (ktl var)) (Vcons (enc (ktl var)) d)) = Some (Vcons Vnil Vnil).
Proof. reflexivity. Qed.

Example self_knil : run sint 15 (Vcons (enc knil) (Vcons (enc knil) d)) = Some Vnil.
Proof. reflexivity. Qed.

Example self_khd  : run sint 20 (Vcons (enc (khd var)) (Vcons (enc (khd var)) d)) = Some Vnil.
Proof. reflexivity. Qed.

Example self_kcons : run sint 25 (Vcons (enc (kcons var var)) (Vcons (enc (kcons var var)) d)) = Some (Vcons d d).
Proof. reflexivity. Qed.

Example self_kifnil :
  run sint 25 (Vcons (enc (kifnil var knil var)) (Vcons (enc (kifnil var knil var)) d)) = Some d.
Proof. reflexivity. Qed.

(** RECURSION via call: drain p = if var then nil else p(tl var) to nil. *)
Definition drain : Exp := kifnil var knil (call (ktl var)).

Example self_drain :
  run sint 80 (Vcons (enc drain) (Vcons (enc drain) (Vcons Vnil Vnil))) = Some Vnil.
Proof. reflexivity. Qed.
