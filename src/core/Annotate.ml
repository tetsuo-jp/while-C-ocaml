(* Annotate - the BTA ANNOTATOR: the first half of an OFFLINE specializer.  Given
 * a core-WHILE program in programs-as-data form ⌜p⌝, it runs the binding-time
 * analysis (the same monovariant BTA as Bta.ml / Cogen.ml) and emits ⌜p_ann⌝: the
 * SAME program with every construct suffix-tagged S (static, reduce) or D (dynamic,
 * residualize), so that an annotation-driven specializer (spec.while, Phase B) can
 * dispatch purely on the program annotation -- never on a runtime value's tag,
 * which is what makes the specializer self-applicable to SMALL compilers.
 *
 * Tagging (the structure is unchanged, only head operators are renamed):
 *   commands     'asgn -> 'asgnS|'asgnD (by target var bt)
 *                'if   -> 'ifS|'ifD     (by guard bt)
 *                'while-> 'whileS|'whileD (by guard bt)
 *                'semi -> 'semi          (no binding time)
 *   expressions  'var/'hd/'tl/'cons/'eq -> …S|…D  (by the node's bt_exp)
 *                'quote -> 'quoteS       (a literal is always static)
 * The program triple's read/write variables ((var i) … (var j)) are left plain.
 * erase strips the suffixes back, recovering ⌜p⌝ exactly (the faithfulness test).
 *
 * usage: annotate <prog.data> [div]      -> ⌜p_ann⌝   (div = SD default | D | S)
 *        annotate --erase   <ann.data>   -> ⌜p⌝       (strip tags)
 *        annotate --reprint <prog.data>  -> ⌜p⌝       (identity, for byte-compare)
 *)
open AbsWhile

let parseValT (c : in_channel) : valT =
  ParWhile.pValT LexWhile.token (Lexing.from_channel c)
let showValT (t : valT) : string = PrintWhile.printTree PrintWhile.prtValT t
let read_file f = let ch = open_in f in let v = parseValT ch in close_in ch; v

let hd = function VCons (a, _) -> a | _ -> VNil
let tl = function VCons (_, b) -> b | _ -> VNil
let rec depth = function VCons (_, r) -> 1 + depth r | _ -> 0
let a name = VAtom (Atom name)
let l2 x y = VCons (x, VCons (y, VNil))
let l3 x y z = VCons (x, VCons (y, VCons (z, VNil)))

(* ===== binding-time analysis (domain & fixpoint, as in Bta.ml / Cogen.ml) ===== *)
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
let rec bpass under c = match c with   (* `under` = under a dynamic guard (cf Cogen.ml) *)
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           let nb = bjoin (bget k) (bjoin (bt_exp e) (if under then D else S)) in
           if nb <> bget k then (changed := true; Hashtbl.replace benv k nb)
       | "'semi", VCons (c1, VCons (c2, VNil)) -> bpass under c1; bpass under c2
       | "'while", VCons (g, VCons (b, VNil)) -> bpass (under || not (bstatic (bt_exp g))) b
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           let u = under || not (bstatic (bt_exp g)) in bpass u c1; bpass u c2
       | _ -> ())
  | _ -> ()

(* ===== annotation (suffix the head operator with S/D) ===== *)
let suf b = if bstatic b then "S" else "D"
let rec ann_exp e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> l2 (a ("'var" ^ suf (bget (depth k)))) k
       | "'quote", VCons (v, VNil) -> l2 (a "'quoteS") v
       | "'hd", VCons (e1, VNil) -> l2 (a ("'hd" ^ suf (bt_exp e))) (ann_exp e1)
       | "'tl", VCons (e1, VNil) -> l2 (a ("'tl" ^ suf (bt_exp e))) (ann_exp e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) -> l3 (a ("'cons" ^ suf (bt_exp e))) (ann_exp e1) (ann_exp e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) -> l3 (a ("'eq" ^ suf (bt_exp e))) (ann_exp e1) (ann_exp e2)
       | _ -> e)
  | _ -> e
let rec ann_cmd c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           l3 (a ("'asgn" ^ suf (bget k))) (ann_exp tgt) (ann_exp e)
       | "'semi", VCons (c1, VCons (c2, VNil)) -> l3 (a "'semi") (ann_cmd c1) (ann_cmd c2)
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           VCons (a ("'if" ^ suf (bt_exp g)), l3 (ann_exp g) (ann_cmd c1) (ann_cmd c2))
       | "'while", VCons (g, VCons (b, VNil)) ->
           VCons (a ("'while" ^ suf (bt_exp g)), l2 (ann_exp g) (ann_cmd b))
       | _ -> c)
  | _ -> c

(* ===== erase (strip a trailing S/D from a known operator) ===== *)
let bases = ["'asgn"; "'if"; "'while"; "'var"; "'quote"; "'hd"; "'tl"; "'cons"; "'eq"]
let unsuf name =
  let n = String.length name in
  if n >= 1 && (name.[n - 1] = 'S' || name.[n - 1] = 'D')
  then (let b = String.sub name 0 (n - 1) in if List.mem b bases then b else name)
  else name
let rec erase_exp e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match unsuf op, args with
       | "'var", VCons (k, VNil) -> l2 (a "'var") k
       | "'quote", VCons (v, VNil) -> l2 (a "'quote") v          (* leave the value v alone *)
       | "'hd", VCons (e1, VNil) -> l2 (a "'hd") (erase_exp e1)
       | "'tl", VCons (e1, VNil) -> l2 (a "'tl") (erase_exp e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) -> l3 (a "'cons") (erase_exp e1) (erase_exp e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) -> l3 (a "'eq") (erase_exp e1) (erase_exp e2)
       | _ -> e)
  | _ -> e
let rec erase_cmd c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match unsuf op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) -> l3 (a "'asgn") (erase_exp tgt) (erase_exp e)
       | "'semi", VCons (c1, VCons (c2, VNil)) -> l3 (a "'semi") (erase_cmd c1) (erase_cmd c2)
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           VCons (a "'if", l3 (erase_exp g) (erase_cmd c1) (erase_cmd c2))
       | "'while", VCons (g, VCons (b, VNil)) ->
           VCons (a "'while", l2 (erase_exp g) (erase_cmd b))
       | _ -> c)
  | _ -> c

let () =
  match Array.to_list Sys.argv with
  | _ :: "--erase" :: f :: _ ->
      let p = read_file f in
      print_endline (showValT (l3 (hd p) (erase_cmd (hd (tl p))) (hd (tl (tl p)))))
  | _ :: "--reprint" :: f :: _ ->
      let p = read_file f in print_endline (showValT p)
  | _ :: f :: rest ->
      let prog = read_file f in
      let varI = hd prog and body = hd (tl prog) and varJ = hd (tl (tl prog)) in
      let i = depth (hd (tl varI)) in
      let div = match rest with "D" :: _ -> D | "S" :: _ -> S | _ -> C (S, D) in
      Hashtbl.replace benv i div;
      changed := true; while !changed do changed := false; bpass false body done;
      print_endline (showValT (l3 varI (ann_cmd body) varJ))
  | _ -> prerr_endline "usage: annotate <prog.data> [div] | --erase <ann> | --reprint <prog>"; exit 1
