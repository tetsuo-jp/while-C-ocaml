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
 * Control: static guards (S-if/S-while) are unrolled at spec time; DYNAMIC guards
 * (D-if/D-while) are residualized -- the `if`/`while` is kept, both branches / the
 * body are specialized once, and every var the body assigns is dynamicized (the
 * BTA raises assignments under a dynamic guard to D; see `under` / `dynamicize`).
 * Out of scope: branches that diverge in binding shape (errors clearly).
 *
 * usage: cogen <prog.data> <static-input.val>
 *   prog reads In=(s.d); prints ⌜target⌝ with [target](d) = [prog]((s.d)).
 *
 *        cogen --online <prog.data> <static-input.val>
 *   POLYVARIANT / value-driven specialization: decides Static/Dynamic from each
 *   construct's ACTUAL annotated value (av_static, and nil-vs-cons for guards),
 *   not from the conservative monovariant BTA.  This unrolls a statically-known
 *   loop that the BTA wrongly collapses to Dynamic (see test-poly.sh) -- the route
 *   toward Jones-optimal specialization.  Assignments under a residualized dynamic
 *   if/while are always emitted (the `under_dyn` rule), as offline does via the BTA.
 *
 *        cogen --gen <prog.data>
 *   prints ⌜gen⌝, a standalone WHILE program (the generating extension) with
 *   [gen](s) = ⌜target⌝ and [target](d) = [prog]((s.d)) -- the true Futamura
 *   cogen output (gen_int_min.while produced by the transformation).  See run_gen.
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
(* `under` = are we under a DYNAMIC guard?  Then every assignment is dynamicized:
 * an assignment executed conditionally / an unknown number of times has a value
 * the specializer cannot track, so its target must be Dynamic (BTA soundness for
 * dynamic control -- without it, e.g. `while D do X:=cons 'a X` would wrongly keep
 * X static). *)
let rec bpass under c = match c with
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

let makeseq rev = match rev with
  | [] -> let v1 = mkvar 1 in l3 (a "'asgn") v1 v1
  | x :: xs -> List.fold_left (fun acc c -> l3 (a "'semi") c acc) x xs

(* variables a command assigns to (its targets), for dynamicization under D-control *)
let rec assigned_vars c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (_, VNil)) -> [depth (hd (tl tgt))]
       | "'semi", VCons (c1, VCons (c2, VNil)) -> assigned_vars c1 @ assigned_vars c2
       | "'while", VCons (_, VCons (b, VNil)) -> assigned_vars b
       | "'if", VCons (_, VCons (c1, VCons (c2, VNil))) -> assigned_vars c1 @ assigned_vars c2
       | _ -> [])
  | _ -> []
(* before residualizing a dynamic if/while: every non-static var the body assigns
 * must hold its residual reference (var k) (its pre-loop value, if any, was already
 * residualized at its last D-assign, since the BTA makes it globally Dynamic) *)
let dynamicize c = List.iter (fun k ->
  if not (bstatic (bget k)) then Hashtbl.replace store k (Dv (mkvar k))) (assigned_vars c)

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

(* ONLINE mode: decide Static/Dynamic from the ACTUAL annotated value (av_static),
 * not from the conservative monovariant BTA.  This is what lets a statically-known
 * loop guard -- e.g. an interpreter's code stack, which the BTA wrongly collapses to
 * Dynamic -- unroll, the route to Jones-optimal specialization.  Offline (BTA-driven)
 * behaviour is preserved when !online is false. *)
let online = ref false
(* `under_dyn` = currently specializing inside a residualized dynamic if/while.  In
 * online mode every assignment there must be EMITTED, never folded: even a static
 * value assigned under a dynamic guard is conditional, so it has to appear in the
 * residual (offline gets this from the BTA's dynamic-control `under` rule). *)
let under_dyn = ref false

(* residualize a dynamic while `while g do b` over the current store *)
let rec residualize_while g b =
  dynamicize b;                              (* loop-assigned vars -> residual refs *)
  let gv = lift (spec_exp g) in
  let saved = !rc and sud = !under_dyn in
  under_dyn := true;
  rc := []; spec b; let body_res = makeseq !rc in
  under_dyn := sud;
  rc := VCons (a "'while", l2 gv body_res) :: saved

and residualize_if g c1 c2 =
  dynamicize c1; dynamicize c2;
  let gv = lift (spec_exp g) in
  let snap = Hashtbl.copy store in
  let saved = !rc and sud = !under_dyn in
  under_dyn := true;
  rc := []; spec c1; let r1 = makeseq !rc in
  Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) snap;
  rc := []; spec c2; let r2 = makeseq !rc in
  under_dyn := sud;
  rc := VCons (a "'if", l3 gv r1 r2) :: saved

and spec c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           let v = spec_exp e in
           let is_static = if !online then (av_static v && not !under_dyn) else bstatic (bget k) in
           if is_static then Hashtbl.replace store k v                    (* fold *)
           else begin                                                     (* emit *)
             rc := l3 (a "'asgn") (mkvar k) (lift v) :: !rc;
             Hashtbl.replace store k (Dv (mkvar k))
           end
       | "'semi", VCons (c1, VCons (c2, VNil)) -> spec c1; spec c2
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           let gv = spec_exp g in
           if !online then
             (match gv with                       (* control needs only nil-vs-cons *)
              | Sv v -> if v <> VNil then spec c1 else spec c2
              | Cv _ -> spec c1                    (* a cons = statically non-nil *)
              | Dv _ -> residualize_if g c1 c2)    (* dynamic: residualize *)
           else if bstatic (bt_exp g) then
             (if av_val gv <> VNil then spec c1 else spec c2)             (* S-if: unroll *)
           else residualize_if g c1 c2                                    (* D-if: residualize *)
       | "'while", VCons (g, VCons (b, VNil)) ->
           if !online then begin
             (* unroll while the guard is statically non-nil (Sv non-nil, or a Cv cons);
              * stop on Sv nil; residualize when the guard is dynamic *)
             let cont = ref true and n = ref 0 in
             let step () = incr n; if !n > 10000000 then failwith "online loop blew up"; spec b in
             while !cont do
               match spec_exp g with
               | Sv v -> if v <> VNil then step () else cont := false
               | Cv _ -> step ()
               | Dv _ -> residualize_while g b; cont := false
             done
           end else if bstatic (bt_exp g) then
             (let n = ref 0 in
              while av_val (spec_exp g) <> VNil do
                incr n; if !n > 1000000 then failwith "static loop blew up"; spec b done)
           else residualize_while g b
       | _ -> failwith "cogen: bad command")
  | _ -> failwith "cogen: bad command"

let run_spec progpath spath =
  let prog = let ch = open_in progpath in let v = parseValT ch in close_in ch; v in
  let s    = let ch = open_in spath    in let v = parseValT ch in close_in ch; v in
  let varI = hd prog and body = hd (tl prog) and varJ = hd (tl (tl prog)) in
  let i = depth (hd (tl varI)) and j = depth (hd (tl varJ)) in
  Hashtbl.replace benv i (C (S, D));
  changed := true; while !changed do changed := false; bpass false body done;
  (* input In = (s . d): head s static, tail d = the residual's input var (var i) *)
  Hashtbl.replace store i (Cv (Sv s, Dv (mkvar i)));
  spec body;
  let outv = sget j in
  let body' =
    if outv = Dv (mkvar j) then makeseq !rc       (* output already self-ref; RC set it *)
    else makeseq (l3 (a "'asgn") (mkvar j) (lift outv) :: !rc)  (* reconstruct it *)
  in
  print_endline (showValT (l3 (mkvar i) body' (mkvar j)))

(* ===== option (a): emit a standalone GENERATING EXTENSION program ⌜gen⌝ =====
 * Instead of running the specialization against a concrete s (option (b), above),
 * --gen emits gen itself: a core-WHILE program (in programs-as-data form) with
 *   [gen](s) = ⌜target⌝   and   [target](d) = [p]((s.d)).
 * This is the true Futamura cogen output -- the offline compiler as a program,
 * the automation of the hand-derived gen_int_min.while.
 *
 * gen's variables: var 0 = its input s, var 1 = its output ⌜target⌝, var 2 = the
 * residual-command accumulator R; each STATIC p-var gets a fresh gen-var (>=3)
 * holding its value at gen-runtime.  DYNAMIC p-vars need no gen-var: a dynamic
 * var k's residual code is always just (var k), a constant.
 *
 * We reuse the av constructors with a DIFFERENT meaning here: each leaf carries a
 * gen-EXPRESSION (data term).  Sv ge = static, ge evaluates (at gen-runtime) to
 * the value; Dv ge = dynamic, ge evaluates to the residual CODE term; Cv = cons.
 * The operations below (g* / lift_code / g_val) build these gen-expression terms. *)

let gIN = 0 and gOUT = 1 and gR = 2
let gvar k = mkvar k
let gq v = l2 (a "'quote") v                       (* gen-expr: literal value v *)
let ghd e = l2 (a "'hd") e
let gtl e = l2 (a "'tl") e
let gcons e1 e2 = l3 (a "'cons") e1 e2
let geq e1 e2 = l3 (a "'eq") e1 e2
let gasgn lhs e = l3 (a "'asgn") lhs e
let gsemi c1 c2 = l3 (a "'semi") c1 c2
let gif g c1 c2 = VCons (a "'if", l3 g c1 c2)
let gwhile g b = VCons (a "'while", l2 g b)

(* gen-var allocation for static p-vars (dynamic p-vars need none) *)
let gv_map : (int, int) Hashtbl.t = Hashtbl.create 16
let gv_next = ref 3
let genvar k = match Hashtbl.find_opt gv_map k with
  | Some g -> g
  | None -> let g = !gv_next in incr gv_next; Hashtbl.replace gv_map k g; g

let rec g_all_static = function Sv _ -> true | Dv _ -> false | Cv (x, y) -> g_all_static x && g_all_static y
let rec g_val = function                            (* gav -> gen-expr computing the static value *)
  | Sv ge -> ge
  | Cv (x, y) -> gcons (g_val x) (g_val y)
  | Dv _ -> failwith "cogen --gen: dynamic value used in static context"
let rec lift_code = function                        (* gav -> gen-expr building the residual code term *)
  | Sv ge -> gcons (gq (a "'quote")) (gcons ge (gq VNil))
  | Dv ge -> ge
  | Cv (x, y) -> gcons (gq (a "'cons")) (gcons (lift_code x) (gcons (lift_code y) (gq VNil)))
let g_hd = function
  | Cv (x, _) -> x
  | Sv ge -> Sv (ghd ge)
  | Dv ge -> Dv (gcons (gq (a "'hd")) (gcons ge (gq VNil)))
let g_tl = function
  | Cv (_, y) -> y
  | Sv ge -> Sv (gtl ge)
  | Dv ge -> Dv (gcons (gq (a "'tl")) (gcons ge (gq VNil)))
let g_eq x y =
  if g_all_static x && g_all_static y then Sv (geq (g_val x) (g_val y))
  else Dv (gcons (gq (a "'eq")) (gcons (lift_code x) (gcons (lift_code y) (gq VNil))))

let gslot : (int, av) Hashtbl.t = Hashtbl.create 64
let gsget k = match Hashtbl.find_opt gslot k with
  | Some v -> v
  | None -> if bstatic (bget k) then Sv (gvar (genvar k))   (* static: lives in a gen-var *)
            else Dv (gq (mkvar k))                          (* dynamic: residual is (var k) *)

let rec g_av e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> gsget (depth k)
       | "'quote", VCons (d, VNil) -> Sv (gq d)
       | "'hd", VCons (e1, VNil) -> g_hd (g_av e1)
       | "'tl", VCons (e1, VNil) -> g_tl (g_av e1)
       | "'cons", VCons (e1, VCons (e2, VNil)) -> Cv (g_av e1, g_av e2)
       | "'eq", VCons (e1, VCons (e2, VNil)) -> g_eq (g_av e1) (g_av e2)
       | _ -> failwith "cogen --gen: bad expr")
  | _ -> failwith "cogen --gen: bad expr"

let gslot_snapshot () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) gslot [] |> List.sort compare
let gslot_restore snap = Hashtbl.clear gslot; List.iter (fun (k, v) -> Hashtbl.replace gslot k v) snap

(* append residual command (the gen-expr `rcmd`) to accumulator gen-var `acc` *)
let gappend acc rcmd = gasgn (gvar acc) (gcons (gq (a "'semi")) (gcons (gvar acc) (gcons rcmd (gq VNil))))
let gskip = ref VNil                              (* a harmless residual skip, set in run_gen *)
let fresh_gv () = let g = !gv_next in incr gv_next; g
(* under dynamic control: every non-static var the body assigns must hold its
 * residual reference (var k) in gslot before we residualize (cf dynamicize, run_spec) *)
let gdynamicize c = List.iter (fun k ->
  if not (bstatic (bget k)) then Hashtbl.replace gslot k (Dv (gq (mkvar k)))) (assigned_vars c)

(* gemit acc c : the gen-code that, run on s, appends c's residual to gen-var `acc` *)
let rec gemit acc c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           let v = g_av e in
           if bstatic (bget k) then begin                  (* S-assign: update gen-var g[k] *)
             let g = genvar k in
             Hashtbl.replace gslot k (Sv (gvar g));
             gasgn (gvar g) (g_val v)
           end else begin                                  (* D/C-assign: append residual command *)
             let rcmd = gcons (gq (a "'asgn")) (gcons (gq (mkvar k)) (gcons (lift_code v) (gq VNil))) in
             Hashtbl.replace gslot k (Dv (gq (mkvar k)));
             gappend acc rcmd
           end
       | "'semi", VCons (c1, VCons (c2, VNil)) ->
           let x = gemit acc c1 in let y = gemit acc c2 in gsemi x y  (* force L-to-R: gemit mutates gslot *)
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           if bstatic (bt_exp g) then begin                (* S-if: a real if in gen *)
             let gg = g_val (g_av g) in                    (* guard read with pre-branch slots *)
             let snap = gslot_snapshot () in
             let g1 = gemit acc c1 in let s1 = gslot_snapshot () in
             gslot_restore snap;
             let g2 = gemit acc c2 in let s2 = gslot_snapshot () in
             if s1 <> s2 then
               failwith "cogen --gen: S-if branches diverge in binding shape (out of scope)";
             gif gg g1 g2
           end else begin                                  (* D-if: gen builds a residual `if` *)
             gdynamicize c1; gdynamicize c2;
             let guard = lift_code (g_av g) in
             let snap = gslot_snapshot () in
             let a1 = fresh_gv () and a2 = fresh_gv () in
             let init1 = gasgn (gvar a1) (gq !gskip) and init2 = gasgn (gvar a2) (gq !gskip) in
             let b1 = gemit a1 c1 in let s1 = gslot_snapshot () in
             gslot_restore snap;
             let b2 = gemit a2 c2 in let s2 = gslot_snapshot () in
             if s1 <> s2 then
               failwith "cogen --gen: D-if branches diverge in binding shape (out of scope)";
             let ifterm = gcons (gq (a "'if")) (gcons guard (gcons (gvar a1) (gcons (gvar a2) (gq VNil)))) in
             gsemi init1 (gsemi b1 (gsemi init2 (gsemi b2 (gappend acc ifterm))))
           end
       | "'while", VCons (g, VCons (b, VNil)) ->
           if bstatic (bt_exp g) then begin                (* S-while: a real while in gen *)
             let gg = g_val (g_av g) in
             let snap = gslot_snapshot () in
             let gb = gemit acc b in
             if snap <> gslot_snapshot () then
               failwith "cogen --gen: S-while changes binding shape (out of scope)";
             gwhile gg gb
           end else begin                                  (* D-while: gen builds a residual `while` *)
             gdynamicize b;
             let guard = lift_code (g_av g) in
             let snap = gslot_snapshot () in
             let sa = fresh_gv () in
             let init = gasgn (gvar sa) (gq !gskip) in
             let gb = gemit sa b in
             if snap <> gslot_snapshot () then
               failwith "cogen --gen: D-while changes binding shape (out of scope)";
             let whterm = gcons (gq (a "'while")) (gcons guard (gcons (gvar sa) (gq VNil))) in
             gsemi init (gsemi gb (gappend acc whterm))
           end
       | _ -> failwith "cogen --gen: bad command")
  | _ -> failwith "cogen --gen: bad command"

let run_gen () =
  let prog = let ch = open_in Sys.argv.(2) in let v = parseValT ch in close_in ch; v in
  let varI = hd prog and body = hd (tl prog) and varJ = hd (tl (tl prog)) in
  let i = depth (hd (tl varI)) and j = depth (hd (tl varJ)) in
  Hashtbl.replace benv i (C (S, D));
  changed := true; while !changed do changed := false; bpass false body done;
  (* the partially-static input: head static = gen's own input (var gIN),
   * tail dynamic = the residual's input var (var i) *)
  Hashtbl.replace gslot i (Cv (Sv (gvar gIN), Dv (gq (mkvar i))));
  gskip := l3 (a "'asgn") (mkvar i) (mkvar i);    (* harmless residual skip (var i):=(var i) *)
  (* prologue: R := a single skip command (harmless seed) *)
  let prologue = gasgn (gvar gR) (gq !gskip) in
  let walked = gemit gR body in
  (* epilogue: unless the output var is already a self-referential dynamic var,
   * append a final command (var j) := <residual code of its current value> *)
  let outv = gsget j in
  let epilogue_app =
    if outv = Dv (gq (mkvar j)) then None
    else Some (gappend gR (gcons (gq (a "'asgn")) (gcons (gq (mkvar j)) (gcons (lift_code outv) (gq VNil))))) in
  (* assemble ⌜target⌝ = ((var i) R (var j)) into gen's output var *)
  let out_target = gasgn (gvar gOUT)
      (gcons (gq (mkvar i)) (gcons (gvar gR) (gcons (gq (mkvar j)) (gq VNil)))) in
  let tail = match epilogue_app with Some e -> gsemi e out_target | None -> out_target in
  let genbody = gsemi prologue (gsemi walked tail) in
  print_endline (showValT (l3 (gvar gIN) genbody (gvar gOUT)))

let () =
  if Array.length Sys.argv >= 2 && Sys.argv.(1) = "--gen" then run_gen ()
  else if Array.length Sys.argv >= 4 && Sys.argv.(1) = "--online" then
    (online := true; run_spec Sys.argv.(2) Sys.argv.(3))
  else run_spec Sys.argv.(1) Sys.argv.(2)
