(* Simp - a semantics-preserving simplifier for residual programs in the
 * programs-as-data form ⌜p⌝ (mix.while output).  Two passes, both sound because
 * WHILE expressions are pure (side-effect free):
 *   1. no-op self-assignment removal `(var k):=(var k)` + `'semi` flattening;
 *   2. global dead-store elimination: drop `(var k):=e` when variable k is
 *      NEVER read in any expression position anywhere (and k is not the output
 *      variable) -- its value cannot affect the result.
 * It does NOT change the interpreting-compiler nature of comp2/comp3 (that needs
 * PAT-READ-ITER), but shrinks the residual measurably.
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

(* variable index depth: var index is nil^k *)
let rec depth = function VCons (_, r) -> 1 + depth r | _ -> 0

module IS = Set.Make (Int)

(* var indices read in EXPRESSION positions *)
let rec reads_exp e acc =
  match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> IS.add (depth k) acc
       | "'quote", VCons (_, VNil) -> acc
       | "'hd", VCons (e1, VNil) -> reads_exp e1 acc
       | "'tl", VCons (e1, VNil) -> reads_exp e1 acc
       | "'cons", VCons (e1, VCons (e2, VNil)) -> reads_exp e2 (reads_exp e1 acc)
       | "'eq", VCons (e1, VCons (e2, VNil)) -> reads_exp e2 (reads_exp e1 acc)
       | _ -> acc)
  | _ -> acc

(* var indices read anywhere in a command (assignment targets do NOT count) *)
let rec reads_cmd c acc =
  match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (_tgt, VCons (e, VNil)) -> reads_exp e acc
       | "'semi", VCons (c1, VCons (c2, VNil)) -> reads_cmd c2 (reads_cmd c1 acc)
       | "'while", VCons (e, VCons (b, VNil)) -> reads_cmd b (reads_exp e acc)
       | "'if", VCons (e, VCons (c1, VCons (c2, VNil))) ->
           reads_cmd c2 (reads_cmd c1 (reads_exp e acc))
       | _ -> acc)
  | _ -> acc

(* a no-op self-assignment (var k) := (var k) *)
let noop = l3 (a "'asgn") (l2 (a "'var") (VCons (VNil, VNil))) (l2 (a "'var") (VCons (VNil, VNil)))

(* simplify a command -> Some c' (kept) | None (droppable).  `reads` = the set of
 * variable depths read somewhere in the whole program (plus the output var). *)
let rec simpC reads c =
  match c with
  | VCons (h, VCons (tgt, VCons (e, VNil))) when h = a "'asgn" ->
      if is_var tgt && tgt = e then None                    (* no-op self-assign *)
      else if is_var tgt && not (IS.mem (depth (hd (tl tgt))) reads) then None
                                                            (* dead store *)
      else Some c
  | VCons (h, VCons (c1, VCons (c2, VNil))) when h = a "'semi" ->
      (match simpC reads c1, simpC reads c2 with
       | None, None -> None
       | Some x, None -> Some x
       | None, Some y -> Some y
       | Some x, Some y -> Some (l3 (a "'semi") x y))
  | VCons (h, VCons (e, VCons (body, VNil))) when h = a "'while" ->
      let b = match simpC reads body with Some x -> x | None -> noop in
      Some (l3 (a "'while") e b)
  | VCons (h, VCons (e, VCons (c1, VCons (c2, VNil)))) when h = a "'if" ->
      let f x = match simpC reads x with Some y -> y | None -> noop in
      Some (VCons (a "'if", VCons (e, VCons (f c1, VCons (f c2, VNil)))))
  | _ -> Some c

let run prog =
  let varI = hd prog in
  let body = hd (tl prog) in
  let varJ = hd (tl (tl prog)) in
  (* the output variable is always considered read *)
  let reads = reads_cmd body (IS.singleton (depth (hd (tl varJ)))) in
  let body' = match simpC reads body with Some x -> x | None -> noop in
  l3 varI body' varJ

let () =
  let prog =
    let ch = open_in Sys.argv.(1) in
    let v = parseValT ch in
    close_in ch; v
  in
  print_endline (showValT (run prog))
