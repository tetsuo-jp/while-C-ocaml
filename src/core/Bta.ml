(* Bta - monovariant binding-time analysis (BTA) for core WHILE programs in the
 * programs-as-data form ⌜p⌝.  The first half of an OFFLINE partial evaluator:
 * it decides, BEFORE specialization and WITHOUT looking at concrete values,
 * which constructs are Static (reduce) and which are Dynamic (residualize),
 * from a binding-time DIVISION of the input.  An offline specializer then just
 * follows these annotations -- the key to a self-applicable, Jones-optimal mix.
 *
 * Binding-time domain (supports PARTIALLY-static structures, needed because the
 * input In = (s . d) is part static / part dynamic):
 *     S            fully static (bottom)
 *     D            dynamic (top)
 *     C(b1,b2)     a cons that is static in b1, dynamic in b2
 * join: S⊔x=x, D⊔x=D, C(a,b)⊔C(c,d)=C(a⊔c,b⊔d).
 *
 * Division: the read variable starts at C(S,D) (head s static, tail d dynamic),
 * matching mix.while's contract; all other variables start at S and are raised
 * to a fixpoint by their assignments.
 *
 * usage: bta <prog.data> [div]   div = SD (default, input (s.d)) | D (fully
 *                                 dynamic) | S (fully static).  Prints each
 *                                 variable's binding time and, for each
 *                                 conditional/loop, whether its guard is S or D
 *                                 (S-if = unroll at spec time, D-if = residualize).
 *)
open AbsWhile

let parseValT (c : in_channel) : valT =
  ParWhile.pValT LexWhile.token (Lexing.from_channel c)

let hd = function VCons (a, _) -> a | _ -> VNil
let tl = function VCons (_, b) -> b | _ -> VNil
let rec depth = function VCons (_, r) -> 1 + depth r | _ -> 0

type bt = S | D | C of bt * bt

let rec join x y =
  match x, y with
  | S, b | b, S -> b
  | D, _ | _, D -> D
  | C (a, b), C (c, d) -> C (join a c, join b d)

let rec fully_static = function S -> true | D -> false | C (a, b) -> fully_static a && fully_static b
let bt_hd = function C (a, _) -> a | S -> S | D -> D
let bt_tl = function C (_, b) -> b | S -> S | D -> D

let env : (int, bt) Hashtbl.t = Hashtbl.create 64
let get k = match Hashtbl.find_opt env k with Some b -> b | None -> S
let raise_to k b =
  let nb = join (get k) b in
  let changed = nb <> get k in
  Hashtbl.replace env k nb; changed

let a name = VAtom (Atom name)

(* binding time of an expression under the current env *)
let rec bt_exp e =
  match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> get (depth k)
       | "'quote", VCons (_, VNil) -> S
       | "'hd", VCons (e1, VNil) -> bt_hd (bt_exp e1)
       | "'tl", VCons (e1, VNil) -> bt_tl (bt_exp e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) -> C (bt_exp e1, bt_exp e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) ->
           if fully_static (bt_exp e1) && fully_static (bt_exp e2) then S else D
       | _ -> D)
  | _ -> D

(* one pass over the commands, raising variable bts; returns whether anything changed.
 * `under` = under a DYNAMIC guard: then every assignment is dynamicized (its target
 * raised to D), because an assignment executed conditionally / an unknown number of
 * times has a value the specializer cannot track (BTA soundness for dynamic control;
 * e.g. without it `while D do X:=cons 'a X` would wrongly keep X static). *)
let changed = ref false
let rec pass_cmd under c =
  match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) when (match tgt with VCons (VAtom (Atom "'var"), _) -> true | _ -> false) ->
           let k = depth (hd (tl tgt)) in
           if raise_to k (join (bt_exp e) (if under then D else S)) then changed := true
       | "'semi", VCons (c1, VCons (c2, VNil)) -> pass_cmd under c1; pass_cmd under c2
       | "'while", VCons (g, VCons (b, VNil)) -> pass_cmd (under || not (fully_static (bt_exp g))) b
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           let u = under || not (fully_static (bt_exp g)) in pass_cmd u c1; pass_cmd u c2
       | _ -> ())
  | _ -> ()

(* report guards once env is stable *)
let guards = ref []
let rec report_cmd c =
  match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'while", VCons (g, VCons (b, VNil)) ->
           guards := ("while", fully_static (bt_exp g)) :: !guards; report_cmd b
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           guards := ("if", fully_static (bt_exp g)) :: !guards; report_cmd c1; report_cmd c2
       | "'semi", VCons (c1, VCons (c2, VNil)) -> report_cmd c1; report_cmd c2
       | _ -> ())
  | _ -> ()

let rec show_bt = function S -> "S" | D -> "D" | C (a, b) -> "(" ^ show_bt a ^ "." ^ show_bt b ^ ")"

let () =
  let prog = let ch = open_in Sys.argv.(1) in let v = parseValT ch in close_in ch; v in
  let varI = hd prog and body = hd (tl prog) and varJ = hd (tl (tl prog)) in
  let i = depth (hd (tl varI)) and j = depth (hd (tl varJ)) in
  let div = if Array.length Sys.argv > 2 then Sys.argv.(2) else "SD" in
  let dbt = match div with "D" -> D | "S" -> S | _ -> C (S, D) in
  Hashtbl.replace env i dbt;                        (* binding-time division of the input *)
  changed := true;
  while !changed do changed := false; pass_cmd false body done;
  report_cmd body;
  Printf.printf "binding-time division: read var %d = (S.D)\n" i;
  Hashtbl.fold (fun k b acc -> (k, b) :: acc) env []
  |> List.sort compare
  |> List.iter (fun (k, b) -> Printf.printf "  var %d : %s\n" k (show_bt b));
  Printf.printf "output var %d : %s\n" j (show_bt (get j));
  List.rev !guards |> List.iteri (fun n (kind, st) ->
    Printf.printf "  %s #%d : %s-%s\n" kind n (if st then "S" else "D") kind)
