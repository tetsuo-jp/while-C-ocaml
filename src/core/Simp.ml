(* Simp - a semantics-preserving simplifier for residual programs in the
 * programs-as-data form ⌜p⌝ (mix.while output).  mix's emit-on-dynamic-
 * assignment model and DYNAMICIZE-ALL produce many no-op self-assignments
 * `(var k) := (var k)` (e.g. from skip encodings) and `'semi` chains around
 * them; this pass removes them.  It does NOT change the interpreting-compiler
 * nature of comp2/comp3 (that needs PAT-READ-ITER / store sizing), but it
 * shrinks the residual measurably.
 *
 * usage: simp <prog.data>      -- prints the simplified ⌜p⌝
 *)
open AbsWhile

let parseValT (c : in_channel) : valT =
  ParWhile.pValT LexWhile.token (Lexing.from_channel c)
let showValT (t : valT) : string =
  PrintWhile.printTree PrintWhile.prtValT t

let hd = function VCons (a, _) -> a | _ -> VNil
let tl = function VCons (_, b) -> b | _ -> VNil
let a name = VAtom (Atom name)
let l2 x y = VCons (x, VCons (y, VNil))
let l3 x y z = VCons (x, VCons (y, VCons (z, VNil)))

let is_var = function
  | VCons (h, VCons (_, VNil)) when h = a "'var" -> true
  | _ -> false

(* a no-op self-assignment (var k) := (var k) *)
let noop = l3 (a "'asgn") (l2 (a "'var") (VCons (VNil, VNil))) (l2 (a "'var") (VCons (VNil, VNil)))

(* simplify a command -> Some c' (kept) | None (a no-op, droppable) *)
let rec simpC c =
  match c with
  | VCons (h, VCons (tgt, VCons (e, VNil))) when h = a "'asgn" ->
      if is_var tgt && tgt = e then None          (* (var k) := (var k) *)
      else Some c
  | VCons (h, VCons (c1, VCons (c2, VNil))) when h = a "'semi" ->
      (match simpC c1, simpC c2 with
       | None, None -> None
       | Some x, None -> Some x
       | None, Some y -> Some y
       | Some x, Some y -> Some (l3 (a "'semi") x y))
  | VCons (h, VCons (e, VCons (body, VNil))) when h = a "'while" ->
      (* keep the loop; an empty body would change termination, so default to a
         no-op body if the body simplified away *)
      let b = match simpC body with Some x -> x | None -> noop in
      Some (l3 (a "'while") e b)
  | VCons (h, VCons (e, VCons (c1, VCons (c2, VNil)))) when h = a "'if" ->
      let f x = match simpC x with Some y -> y | None -> noop in
      Some (VCons (a "'if", VCons (e, VCons (f c1, VCons (f c2, VNil)))))
  | _ -> Some c

let run prog =
  let varI = hd prog in
  let body = hd (tl prog) in
  let varJ = hd (tl (tl prog)) in
  let body' = match simpC body with Some x -> x | None -> noop in
  l3 varI body' varJ

let () =
  let prog =
    let ch = open_in Sys.argv.(1) in
    let v = parseValT ch in
    close_in ch; v
  in
  print_endline (showValT (run prog))
