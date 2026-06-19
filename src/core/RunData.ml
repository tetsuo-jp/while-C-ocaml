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

(* store: a MUTABLE int-keyed hashtable, keyed by the variable index DEPTH k of
 * nil^k -- O(1) access, no per-step allocation.  (Running a residual ⌜p⌝ is
 * ordinary imperative execution: mix's specialization-time snapshots/merges are
 * already baked into the residual as plain asgn/semi/while/if, so an in-place
 * store is correct here.)  Earlier: assoc list keyed by structural nil^k
 * equality = O(#vars * index_len) per access; then a persistent Map = O(log n)
 * with allocation.  This is the fastest of the three. *)
let rec depth = function VCons (_, r) -> 1 + depth r | _ -> 0
let lookup st kidx =
  match Hashtbl.find_opt st (depth kidx) with Some v -> v | None -> VNil
let update st kidx v = Hashtbl.replace st (depth kidx) v

(* Dispatch on the node's head atom once (string match), instead of re-building
 * a VAtom and structurally comparing per node -- the per-node hot path. *)
let rec evalE st e =
  match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> lookup st k
       | "'quote", VCons (d, VNil) -> d
       | "'hd", VCons (e1, VNil) -> hd (evalE st e1)
       | "'tl", VCons (e1, VNil) -> tl (evalE st e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) ->
           VCons (evalE st e1, evalE st e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) ->
           if evalE st e1 = evalE st e2 then VCons (VNil, VNil) else VNil
       | _ -> failwith ("rundata: bad expression: " ^ showValT e))
  | _ -> failwith ("rundata: bad expression: " ^ showValT e)

let rec evalC st c =
  match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (VCons (VAtom (Atom "'var"), VCons (k, VNil)), VCons (e, VNil)) ->
           update st k (evalE st e)
       | "'semi", VCons (c1, VCons (c2, VNil)) -> evalC st c1; evalC st c2
       | "'while", VCons (e, VCons (c1, VNil)) ->
           while evalE st e <> VNil do evalC st c1 done
       | "'if", VCons (e, VCons (c1, VCons (c2, VNil))) ->
           if evalE st e <> VNil then evalC st c1 else evalC st c2
       | _ -> failwith ("rundata: bad command: " ^ showValT c))
  | _ -> failwith ("rundata: bad command: " ^ showValT c)

let run prog input =
  let varI = hd prog in
  let c = hd (tl prog) in
  let varJ = hd (tl (tl prog)) in
  let i = hd (tl varI) in
  let j = hd (tl varJ) in
  let st : (int, valT) Hashtbl.t = Hashtbl.create 1024 in
  update st i input;
  evalC st c;
  lookup st j

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
