(* Cogen - an OFFLINE partial evaluator for core WHILE in programs-as-data form,
 * driven by binding-time ANNOTATIONS (BTA, as in Bta.ml) rather than by online
 * value inference.  It auto-produces the SMALL specialized target that the
 * hand-derived generating extension gen_int_min.while emits -- the automation of
 * the offline pipeline (BTA -> annotation-driven specialization -> small target).
 *
 * Static constructs are reduced; dynamic ones are residualized.  Values carry a
 * partially-static annotated value (Sv / Dv / Cv) so the input In = (s.d) -- head
 * static, tail dynamic -- is handled correctly (tl In residualizes to the
 * residual's input variable).
 *
 * Scope: static control (S-if/S-while are unrolled at spec time).  Dynamic
 * control (D-if/D-while) needs the snapshot/merge + DYNAMICIZE-ALL generation of
 * mix.while and is out of scope (errors out clearly).
 *
 * usage: cogen <prog.data> <static-input.val>
 *   prog reads In=(s.d); prints ⌜target⌝ with [target](d) = [prog]((s.d)).
 *)
open AbsWhile

let parseValT (c : in_channel) : valT =
  ParWhile.pValT LexWhile.token (Lexing.from_channel c)
let showValT (t : valT) : string = PrintWhile.printTree PrintWhile.prtValT t

let hd = function VCons (a, _) -> a | _ -> VNil
let tl = function VCons (_, b) -> b | _ -> VNil
let rec depth = function VCons (_, r) -> 1 + depth r | _ -> 0
let a name = VAtom (Atom name)
let l2 x y = VCons (x, VCons (y, VNil))
let l3 x y z = VCons (x, VCons (y, VCons (z, VNil)))
let mkvar k = let rec nils n = if n = 0 then VNil else VCons (VNil, nils (n - 1)) in
              l2 (a "'var") (nils k)

(* ===== binding-time analysis (domain & fixpoint, as in Bta.ml) ===== *)
type bt = S | D | C of bt * bt
let rec bjoin x y = match x, y with
  | S, b | b, S -> b | D, _ | _, D -> D | C (a, b), C (c, d) -> C (bjoin a c, bjoin b d)
let rec bstatic = function S -> true | D -> false | C (a, b) -> bstatic a && bstatic b
let bhd = function C (a, _) -> a | S -> S | D -> D
let btl = function C (_, b) -> b | S -> S | D -> D
let benv : (int, bt) Hashtbl.t = Hashtbl.create 64
let bget k = match Hashtbl.find_opt benv k with Some b -> b | None -> S
let rec bt_exp e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> bget (depth k)
       | "'quote", _ -> S
       | "'hd", VCons (e1, VNil) -> bhd (bt_exp e1)
       | "'tl", VCons (e1, VNil) -> btl (bt_exp e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) -> C (bt_exp e1, bt_exp e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) ->
           if bstatic (bt_exp e1) && bstatic (bt_exp e2) then S else D
       | _ -> D)
  | _ -> D
let changed = ref false
let rec bpass c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           let nb = bjoin (bget k) (bt_exp e) in
           if nb <> bget k then (changed := true; Hashtbl.replace benv k nb)
       | "'semi", VCons (c1, VCons (c2, VNil)) -> bpass c1; bpass c2
       | "'while", VCons (_, VCons (b, VNil)) -> bpass b
       | "'if", VCons (_, VCons (c1, VCons (c2, VNil))) -> bpass c1; bpass c2
       | _ -> ())
  | _ -> ()

(* ===== annotated values ===== *)
type av = Sv of valT | Dv of valT | Cv of av * av
let rec lift = function
  | Sv v -> l2 (a "'quote") v
  | Dv c -> c
  | Cv (x, y) -> l3 (a "'cons") (lift x) (lift y)
let rec av_static = function Sv _ -> true | Dv _ -> false | Cv (x, y) -> av_static x && av_static y
let rec av_val = function
  | Sv v -> v | Cv (x, y) -> VCons (av_val x, av_val y) | Dv _ -> failwith "av_val: dynamic"
let av_hd = function Cv (x, _) -> x | Sv v -> Sv (hd v) | Dv c -> Dv (l2 (a "'hd") c)
let av_tl = function Cv (_, y) -> y | Sv v -> Sv (tl v) | Dv c -> Dv (l2 (a "'tl") c)
let av_eq x y =
  if av_static x && av_static y then Sv (if av_val x = av_val y then VCons (VNil, VNil) else VNil)
  else Dv (l3 (a "'eq") (lift x) (lift y))

(* ===== specialization ===== *)
let store : (int, av) Hashtbl.t = Hashtbl.create 64
let sget k = match Hashtbl.find_opt store k with Some v -> v | None -> Sv VNil
let rc = ref []                                  (* residual commands, reversed *)

let rec spec_exp e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> sget (depth k)
       | "'quote", VCons (d, VNil) -> Sv d
       | "'hd", VCons (e1, VNil) -> av_hd (spec_exp e1)
       | "'tl", VCons (e1, VNil) -> av_tl (spec_exp e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) -> Cv (spec_exp e1, spec_exp e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) -> av_eq (spec_exp e1) (spec_exp e2)
       | _ -> failwith "cogen: bad expr")
  | _ -> failwith "cogen: bad expr"

let rec spec c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           let v = spec_exp e in
           if bstatic (bget k) then Hashtbl.replace store k v              (* S-assign: fold *)
           else begin                                                      (* D-assign: emit *)
             rc := l3 (a "'asgn") (mkvar k) (lift v) :: !rc;
             Hashtbl.replace store k (Dv (mkvar k))
           end
       | "'semi", VCons (c1, VCons (c2, VNil)) -> spec c1; spec c2
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           if bstatic (bt_exp g) then
             (if av_val (spec_exp g) <> VNil then spec c1 else spec c2)    (* S-if: unroll *)
           else failwith "cogen: dynamic if (D-if) out of scope (needs merge, cf mix.while)"
       | "'while", VCons (g, VCons (b, VNil)) ->
           if bstatic (bt_exp g) then
             (let n = ref 0 in
              while av_val (spec_exp g) <> VNil do
                incr n; if !n > 1000000 then failwith "static loop blew up"; spec b done)
           else failwith "cogen: dynamic while (D-while) out of scope (cf mix.while)"
       | _ -> failwith "cogen: bad command")
  | _ -> failwith "cogen: bad command"

let makeseq rev = match rev with
  | [] -> let v1 = mkvar 1 in l3 (a "'asgn") v1 v1
  | x :: xs -> List.fold_left (fun acc c -> l3 (a "'semi") c acc) x xs

let () =
  let prog = let ch = open_in Sys.argv.(1) in let v = parseValT ch in close_in ch; v in
  let s    = let ch = open_in Sys.argv.(2) in let v = parseValT ch in close_in ch; v in
  let varI = hd prog and body = hd (tl prog) and varJ = hd (tl (tl prog)) in
  let i = depth (hd (tl varI)) and j = depth (hd (tl varJ)) in
  Hashtbl.replace benv i (C (S, D));
  changed := true; while !changed do changed := false; bpass body done;
  (* input In = (s . d): head s static, tail d = the residual's input var (var i) *)
  Hashtbl.replace store i (Cv (Sv s, Dv (mkvar i)));
  spec body;
  let outv = sget j in
  let body' =
    if outv = Dv (mkvar j) then makeseq !rc       (* output already self-ref; RC set it *)
    else makeseq (l3 (a "'asgn") (mkvar j) (lift outv) :: !rc)  (* reconstruct it *)
  in
  print_endline (showValT (l3 (mkvar i) body' (mkvar j)))
