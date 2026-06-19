(* RunData - a native interpreter for the programs-as-data representation ⌜p⌝
 * (the form printed by DesugarWhile --data / --data-if, and produced by
 * mix.while).  It is the OCaml analogue of universal.while but compiled, so it
 * runs LARGE residuals (e.g. the fp2 compiler comp2, tens of MB) fast enough to
 * verify them — universal.while interpreting such a program is infeasible.
 * Mirrors rwhile's `d2p` / direct-eval verification path.
 *
 * Handles every node mix/universal emit: commands 'asgn/'semi/'while/'if and
 * expressions 'var/'quote/'cons/'hd/'tl/'eq.  Variable indices are nil^k values.
 *
 * usage: rundata <prog.data> <input.val>      -- prints ⟦p⟧(input)
 *)
open AbsWhile

let parseValT (c : in_channel) : valT =
  ParWhile.pValT LexWhile.token (Lexing.from_channel c)
let showValT (t : valT) : string =
  PrintWhile.printTree PrintWhile.prtValT t

let hd = function VCons (a, _) -> a | _ -> VNil
let tl = function VCons (_, b) -> b | _ -> VNil

(* store: association list keyed by the variable index value (nil^k) *)
let rec lookup k = function
  | [] -> VNil
  | (k', v) :: r -> if k' = k then v else lookup k r
let rec update k v = function
  | [] -> [ (k, v) ]
  | (k', v') :: r -> if k' = k then (k, v) :: r else (k', v') :: update k v r

let a name = VAtom (Atom name)

let rec evalE store e =
  match e with
  | VCons (h, VCons (k, VNil)) when h = a "'var"   -> lookup k store
  | VCons (h, VCons (d, VNil)) when h = a "'quote" -> d
  | VCons (h, VCons (e1, VNil)) when h = a "'hd"   -> hd (evalE store e1)
  | VCons (h, VCons (e1, VNil)) when h = a "'tl"   -> tl (evalE store e1)
  | VCons (h, VCons (e1, VCons (e2, VNil))) when h = a "'cons" ->
      VCons (evalE store e1, evalE store e2)
  | VCons (h, VCons (e1, VCons (e2, VNil))) when h = a "'eq" ->
      if evalE store e1 = evalE store e2 then VCons (VNil, VNil) else VNil
  | _ -> failwith ("rundata: bad expression: " ^ showValT e)

let rec evalC store c =
  match c with
  | VCons (h, VCons (VCons (hv, VCons (k, VNil)), VCons (e, VNil)))
    when h = a "'asgn" && hv = a "'var" ->
      update k (evalE store e) store
  | VCons (h, VCons (c1, VCons (c2, VNil))) when h = a "'semi" ->
      evalC (evalC store c1) c2
  | VCons (h, VCons (e, VCons (c1, VNil))) when h = a "'while" ->
      let rec loop store =
        if evalE store e = VNil then store else loop (evalC store c1)
      in
      loop store
  | VCons (h, VCons (e, VCons (c1, VCons (c2, VNil)))) when h = a "'if" ->
      if evalE store e = VNil then evalC store c2 else evalC store c1
  | _ -> failwith ("rundata: bad command: " ^ showValT c)

let run prog input =
  let varI = hd prog in
  let c = hd (tl prog) in
  let varJ = hd (tl (tl prog)) in
  let i = hd (tl varI) in
  let j = hd (tl varJ) in
  let store = update i input [] in
  let store = evalC store c in
  lookup j store

let () =
  let prog =
    let ch = open_in Sys.argv.(1) in
    let v = parseValT ch in
    close_in ch; v
  in
  let input =
    let ch = open_in Sys.argv.(2) in
    let v = parseValT ch in
    close_in ch; v
  in
  print_endline (showValT (run prog input))
