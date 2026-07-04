#!/bin/sh
# test-extract.sh - the EXTRACTED verified interpreter, run as OCaml.
#
# `proofs/rocq/Extract.v` extracts the machine-checked Rocq `eval`/`run`/`enc`/
# `decode`/`sint` to `while_eval.ml`.  This test compiles that extracted code and
# runs a battery of I-language (`Exp`) programs, checking:
#   1. the extracted `eval`/`run` computes the right value for every constructor,
#      run DIRECTLY (incl. `call`/recursion via `drain`);
#   2. the extracted self-interpreter `sint` reproduces the SelfInterp.v oracle
#      values on `(enc p . (enc p . inp))`;
#   3. G1 AT RUNTIME -- a direct run and a self-interpretation of the same program
#      agree (SimFFull.simF_full, exhibited executably);
#   4. `decode` is a left inverse of `enc`.
# This ties the Rocq proofs to an executable artefact.  (Self-interpreting a
# RECURSIVE program is left out: the extracted `eval` is strict, so it forces both
# `kifnil` branches -- the deep recursion needed there blows up, though every case
# below is instant.  Recursion is still covered by `direct/drain`.)
#
# Skips cleanly (exit 0) if `rocq` or `ocamlc` is absent, like the optional Agda leg.
set -e
cd "$(dirname "$0")/../.."
command -v rocq   >/dev/null 2>&1 || { echo "(rocq not found; skipping extraction test)"; exit 0; }
command -v ocamlc >/dev/null 2>&1 || { echo "(ocamlc not found; skipping extraction test)"; exit 0; }

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT

# 1. extract while_eval.ml from the verified Rocq development
( cd proofs/rocq && rocq compile -Q . "" Extract.v ) >/dev/null 2>&1 \
  || { echo "FAIL: Extract.v did not compile"; exit 1; }
[ -f proofs/rocq/while_eval.ml ] && [ -f proofs/rocq/while_eval.mli ] \
  || { echo "FAIL: while_eval.ml{,i} not produced"; exit 1; }
cp proofs/rocq/while_eval.ml proofs/rocq/while_eval.mli "$tmp/"

# 2. the OCaml driver: run the extracted interpreter, check against proved oracles
cat > "$tmp/driver.ml" <<'OCAML'
open While_eval
let rec nat_of_int n = if n <= 0 then O else S (nat_of_int (n - 1))
let rec v_eq a b = match a, b with
  | Vnil, Vnil -> true
  | Vcons (a1,a2), Vcons (b1,b2) -> v_eq a1 b1 && v_eq a2 b2 | _,_ -> false
let rec exp_eq a b = match a, b with
  | Var,Var | Knil,Knil -> true
  | Kcons(a1,a2),Kcons(b1,b2) -> exp_eq a1 b1 && exp_eq a2 b2
  | Khd a1,Khd b1 | Ktl a1,Ktl b1 | Call a1,Call b1 -> exp_eq a1 b1
  | Kifnil(a1,a2,a3),Kifnil(b1,b2,b3) -> exp_eq a1 b1 && exp_eq a2 b2 && exp_eq a3 b3
  | _,_ -> false
let rec show = function Vnil -> "nil" | Vcons(a,b) -> "("^show a^" . "^show b^")"
let show_opt = function Some v -> show v | None -> "None"
let fails = ref 0
let check name got want = match got with
  | Some g when v_eq g want -> Printf.printf "ok   %s = %s\n" name (show g)
  | _ -> incr fails; Printf.printf "FAIL %s: got %s, want %s\n" name (show_opt got) (show want)
let d = Vcons (Vnil, Vcons (Vnil, Vnil))                (* [nil, nil] *)
let drain = Kifnil (Var, Knil, Call (Ktl Var))          (* recurse tl until nil *)
let selfrun p inp f = run sint (nat_of_int f) (Vcons (enc p, Vcons (enc p, inp)))
let () =
  (* (1) extracted eval, every constructor, run directly (incl. recursion) *)
  check "direct/var"    (run Var (nat_of_int 5) d) d;
  check "direct/knil"   (run Knil (nat_of_int 5) d) Vnil;
  check "direct/khd"    (run (Khd Var) (nat_of_int 5) d) Vnil;
  check "direct/ktl"    (run (Ktl Var) (nat_of_int 5) d) (Vcons (Vnil, Vnil));
  check "direct/kcons"  (run (Kcons (Ktl Var, Khd Var)) (nat_of_int 5) d) (Vcons (Vcons (Vnil,Vnil), Vnil));
  check "direct/kifnil" (run (Kifnil (Var, Knil, Ktl Var)) (nat_of_int 5) d) (Vcons (Vnil, Vnil));
  check "direct/drain(recursion)" (run drain (nat_of_int 20) (Vcons (Vnil, Vcons (Vnil, Vnil)))) Vnil;
  (* (2) self-interpreter == SelfInterp.v Examples *)
  check "self/var"    (selfrun Var d 15) d;
  check "self/knil"   (selfrun Knil d 15) Vnil;
  check "self/khd"    (selfrun (Khd Var) d 20) Vnil;
  check "self/ktl"    (selfrun (Ktl Var) d 20) (Vcons (Vnil, Vnil));
  check "self/kcons"  (selfrun (Kcons (Var, Var)) d 25) (Vcons (d, d));
  check "self/kifnil" (selfrun (Kifnil (Var, Knil, Var)) d 25) d;
  (* (3) G1 at runtime: direct run == self-interpretation of the same program *)
  let g1 name p inp fd fs = match run p (nat_of_int fd) inp, selfrun p inp fs with
    | Some x, Some y when v_eq x y -> Printf.printf "ok   G1/%s: direct=self=%s\n" name (show x)
    | a, b -> incr fails; Printf.printf "FAIL G1/%s: direct=%s self=%s\n" name (show_opt a) (show_opt b) in
  g1 "khd"   (Khd Var) d 5 20;
  g1 "ktl"   (Ktl Var) d 5 20;
  g1 "kcons" (Kcons (Ktl Var, Khd Var)) d 5 30;
  (* (4) decode is a left inverse of enc *)
  (match decode (enc drain) with
   | Some e when exp_eq e drain -> print_string "ok   decode(enc drain) = drain\n"
   | _ -> incr fails; print_string "FAIL decode(enc drain)\n");
  if !fails = 0 then print_string "extract: PASS\n"
  else (Printf.printf "extract: FAIL (%d checks)\n" !fails; exit 1)
OCAML

# 3. compile the extracted module + driver, run
( cd "$tmp" && ocamlc -w -a while_eval.mli while_eval.ml driver.ml -o driver ) \
  || { echo "FAIL: extracted driver did not compile"; exit 1; }
"$tmp/driver"
