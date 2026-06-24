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
(* WIDENING: the partially-static domain (C) has infinite ascending chains -- e.g.
 * a static-control accumulator `Acc := cons D Acc` makes Acc's type grow
 * C(D, C(D, ...)) without bound, so the naive fixpoint diverges.  `widen` caps the
 * C-nesting at bt_maxdepth (collapsing deeper structure to D) and normalizes
 * C(D,D)->D; both are sound (D is top) and make the bt domain finite, so the BTA
 * always terminates.  This is the binding-time half of online-termination control. *)
let bt_maxdepth = 8
let rec widen d = function
  | C (a, b) ->
      if d <= 0 then D
      else (match widen (d - 1) a, widen (d - 1) b with D, D -> D | a', b' -> C (a', b'))
  | x -> x
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
       | "'cons", VCons (e1, VCons (e2, VNil)) -> widen bt_maxdepth (C (bt_exp e1, bt_exp e2))
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
           let nb = widen bt_maxdepth (bjoin (bget k) (bjoin (bt_exp e) (if under then D else S))) in
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
(* ----- liveness (to make dynamicize precise for online residual loops) -----
 * A variable that a dynamic loop body assigns but that is neither LIVE-IN (read
 * before being assigned in the body) nor LIVE-OUT (read after the loop) does NOT
 * need dynamicizing: its loop-head value is dead (re-initialised each iteration)
 * and its loop-exit value is dead (never read after).  Keeping such a var static
 * lets the body's static dispatch unroll -- e.g. drain.while's `Cd := Body` per
 * iteration -- driving residual interpreter overhead to 0 (full Jones-optimality
 * for looping subjects).  Vars that ARE live-in/live-out stay dynamicized (sound:
 * their value genuinely changes across iterations / is read afterwards). *)
let uni a b = List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) a b
let rec reads_e e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> [depth k]
       | "'hd", VCons (e1, VNil) | "'tl", VCons (e1, VNil) -> reads_e e1
       | "'cons", VCons (e1, VCons (e2, VNil))
       | "'eq", VCons (e1, VCons (e2, VNil)) -> uni (reads_e e1) (reads_e e2)
       | _ -> [])
  | _ -> []
(* (live_in, must_assign): must_assign = definitely-assigned, used to kill in seq *)
let rec lva c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) -> (reads_e e, [depth (hd (tl tgt))])
       | "'semi", VCons (c1, VCons (c2, VNil)) ->
           let (l1, m1) = lva c1 and (l2, m2) = lva c2 in
           (uni l1 (List.filter (fun x -> not (List.mem x m1)) l2), uni m1 m2)
       | "'while", VCons (g, VCons (b, VNil)) ->          (* body may run 0 times *)
           let (lb, _) = lva b in (uni (reads_e g) lb, [])
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           let (l1, m1) = lva c1 and (l2, m2) = lva c2 in
           (uni (reads_e g) (uni l1 l2), List.filter (fun x -> List.mem x m2) m1)
       | _ -> ([], []))
  | _ -> ([], [])
let live_in c = fst (lva c)
(* the continuation of the command currently being specialized, as a stack of
 * pending commands; live_out = vars any continuation reads (conservative) plus
 * the output variable.  Threaded by spec's `'semi` case. *)
let cont_stack : valT list ref = ref []
let out_var = ref (-1)
let live_out () =
  List.fold_left (fun acc c -> uni acc (live_in c))
    (if !out_var >= 0 then [!out_var] else []) !cont_stack

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
(* `dyn_live` = the ESCAPING variables of the dynamic if/while we are currently
 * residualizing: those assigned in it that are live-in (read before assigned, so
 * their value changes across iterations) or live-out (read after the construct).
 * An assignment to such a var must be EMITTED, never folded -- it is conditional /
 * iterated and its value escapes.  An assignment to a var NOT in dyn_live (a dead
 * loop-local, e.g. drain.while's `Cd := Body` re-initialised each iteration) is
 * folded, so the body's static dispatch over it unrolls.  This is the precise
 * (liveness-aware) version of the old all-or-nothing "emit everything under a
 * dynamic guard" rule -- the route to Jones-optimality for looping subjects. *)
let dyn_live : int list ref = ref []
let escaping_set b extra =                          (* live-in(b) | extra, non-static *)
  let live = uni (live_in b) extra in
  List.filter (fun k -> not (bstatic (bget k)) && List.mem k live) (assigned_vars b)

(* online-termination control: bounded unrolling of static-guarded loops, with a
 * GENERALIZATION fallback (residualize the loop, its whole state made dynamic) when
 * the budget is exceeded -- so a loop whose guard the value-driven analysis keeps
 * statically non-nil for too long (a quasi-non-terminating unroll) still produces a
 * finite, correct residual instead of diverging.  Budget is overridable for tests. *)
exception Generalize
let unroll_budget = try int_of_string (Sys.getenv "COGEN_UNROLL_BUDGET") with _ -> 1000000

(* behaviour-NEUTRAL diagnostic for item-3: when COGEN_TRACE is set, report each
 * residualization (guard + escaping set + dyn_live context) so we can see which
 * registers go dynamic inside a residualized loop -- the cause of surviving
 * interpreter overhead.  No effect when the env var is unset. *)
let trace_on = (try Sys.getenv "COGEN_TRACE" <> "" with Not_found -> false)
(* report a residualization event; the thunk defers the (costly) guard/escaping
 * rendering so there is ZERO cost when COGEN_TRACE is unset. *)
let trace_res kind g esc dl =
  if trace_on then
    Printf.eprintf "[%s] guard=%s  esc=[%s]  dyn_live=[%s]\n" kind
      (showValT g)
      (String.concat ";" (List.map string_of_int esc))
      (String.concat ";" (List.map string_of_int dl))

(* ===== item-3 (M1a): configuration memoization & back-edge folding =====
 * Gated behind COGEN_MEMO so the default (green) corpus is untouched while this is
 * developed.  A CONFIGURATION is (program-point, static-store projection): two
 * stores with the same static spine (dynamic leaves abstracted to a single bottom)
 * are the SAME config even if dynamic leaf values differ.  When specialization
 * re-reaches a config it is already inside (a back-edge -- e.g. an interpreter
 * re-reaching a subject `while`), instead of unrolling forever we close a residual
 * `while` (fold, a la supercompilation): the subject's own loop, with the code
 * pointer kept static so the interpreter dispatch inside it unrolls. *)
let memo_on = (try Sys.getenv "COGEN_MEMO" <> "" with Not_found -> false)
(* static projection of an annotated value: the spine with dynamic leaves -> 'D *)
let proj_av =
  let rec p = function
    | Sv v -> l2 (a "'S") v
    | Dv _ -> a "'D"
    | Cv (x, y) -> l3 (a "'C") (p x) (p y) in p
(* canonical projection of the whole store (sorted by var index) = sigma_s *)
let config () : (int * valT) list =
  List.sort compare (Hashtbl.fold (fun k v acc -> (k, proj_av v) :: acc) store [])
(* a configuration = (sigma_s, continuation k): two driving points are the SAME
 * when the static store projection AND the pending continuation match -- a
 * back-edge (the subject loop iterating).  The residual guard is a function of the
 * store, so it need not be in the key. *)
type cfg = (int * valT) list * valT list
exception Fold of cfg
(* loop heads currently being driven (ancestors), for back-edge detection *)
let loop_stack : cfg list ref = ref []
(* drive step budget: a back-edge that never recurs (config keeps growing) would
 * drive forever; cap it and fall back to the non-memo spec. *)
exception MemoBail
let drive_steps = ref 0
let drive_budget = try int_of_string (Sys.getenv "COGEN_MEMO_BUDGET") with _ -> 200000

(* residualize a dynamic while `while g do b` over the current store.  `force` is a
 * set of vars to treat as escaping in addition to the live-in/live-out ones (used by
 * generalization to push the whole loop state dynamic). *)
let rec residualize_while force g b =
  let esc = uni (escaping_set b (live_out ())) force in
  trace_res "res-while" g esc !dyn_live;
  let dyn () = List.iter (fun k -> Hashtbl.replace store k (Dv (mkvar k))) esc in
  dyn ();                                    (* loop-head store: generalise escaping vars *)
  let gv = lift (spec_exp g) in
  let saved = !rc and sdl = !dyn_live in
  dyn_live := esc;
  rc := []; spec b; let body_res = makeseq !rc in
  dyn_live := sdl;
  dyn ();                                    (* post-loop store: escaping vars stay dynamic *)
  rc := VCons (a "'while", l2 gv body_res) :: saved

and residualize_if g c1 c2 =
  (* conservative: every non-static var either branch assigns is treated as escaping
   * (emitted).  Narrowing this with liveness (as residualize_while does) makes the
   * self-interpreter's dispatch blow up COMBINATORIALLY -- many distinct static
   * stores each spawn work -- which per-loop termination control cannot bound; the
   * cure is program-point memoization (sharing specialized configurations), item 3. *)
  let esc = uni (List.filter (fun k -> not (bstatic (bget k))) (assigned_vars c1))
                (List.filter (fun k -> not (bstatic (bget k))) (assigned_vars c2)) in
  trace_res "res-if" g esc !dyn_live;
  List.iter (fun k -> Hashtbl.replace store k (Dv (mkvar k))) esc;
  let gv = lift (spec_exp g) in
  let snap = Hashtbl.copy store in
  let saved = !rc and sdl = !dyn_live in
  dyn_live := esc;
  rc := []; spec c1; let r1 = makeseq !rc in
  Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) snap;
  rc := []; spec c2; let r2 = makeseq !rc in
  dyn_live := sdl;
  rc := VCons (a "'if", l3 gv r1 r2) :: saved

and spec c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) ->
           let k = depth (hd (tl tgt)) in
           let v = spec_exp e in
           let is_static =
             if !online then (av_static v && not (List.mem k !dyn_live))
             else bstatic (bget k) in
           if is_static then Hashtbl.replace store k v                    (* fold *)
           else begin                                                     (* emit *)
             rc := l3 (a "'asgn") (mkvar k) (lift v) :: !rc;
             (* Preserve a partially-static SPINE across the emit: store the slot as
              * a Cv with the same spine but every leaf a SELF-reference into (var k)
              * (hd/tl paths).  Then loops that walk k's spine still unroll, while
              * each leaf reads k's own slot -- stable under re-materialisation, so we
              * keep the emit-model soundness (MixEmitModel: self-ref slots are sound,
              * cross-ref slots are not).  A pure-dynamic value stays Dv (var k). *)
             let rec selfref path = function
               | Cv (x, y) -> Cv (selfref (l2 (a "'hd") path) x, selfref (l2 (a "'tl") path) y)
               | Sv s -> Sv s                  (* keep static parts (incl. spine terminator) *)
               | Dv _ -> Dv path in            (* dynamic leaf -> stable self-reference *)
             (* Preserving the spine is sound only OUTSIDE residualized dynamic control:
              * a self-ref bakes in a fixed spine length, but under a residualized
              * dynamic if/while a variable's runtime spine can vary per iteration, so
              * there we must collapse to a plain Dv.  In straight-line context
              * (dyn_live empty) the spine is fixed, so self-refs keep it static and
              * store-walks unroll -- the route to loop-free (Jones-optimal) residuals. *)
             let slot =
               if !online && !dyn_live = [] then selfref (mkvar k) v else Dv (mkvar k) in
             Hashtbl.replace store k slot
           end
       | "'semi", VCons (c1, VCons (c2, VNil)) ->
           cont_stack := c2 :: !cont_stack; spec c1;
           (match !cont_stack with _ :: t -> cont_stack := t | [] -> ());
           spec c2
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
              * stop on Sv nil; residualize when the guard is dynamic.  If the unroll
              * exceeds the budget, GENERALIZE: roll back, make the whole loop state
              * dynamic, and residualize the loop (guaranteed finite & correct). *)
             let snap_store = Hashtbl.copy store and snap_rc = !rc in
             (try
                let cont = ref true and n = ref 0 in
                let step () = incr n; if !n > unroll_budget then raise Generalize; spec b in
                while !cont do
                  match spec_exp g with
                  | Sv v -> if v <> VNil then step () else cont := false
                  | Cv _ -> step ()
                  | Dv _ -> residualize_while [] g b; cont := false
                done
              with Generalize ->
                Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) snap_store;
                rc := snap_rc;
                (* materialize each still-static loop-carried var's entry value, then
                 * make it dynamic, so the residual loop is self-contained *)
                let carried = assigned_vars b in
                List.iter (fun k -> match sget k with
                  | Dv _ -> ()                         (* already a residual reference *)
                  | av ->                              (* Sv/Cv: bind its entry value, go dynamic *)
                      rc := l3 (a "'asgn") (mkvar k) (lift av) :: !rc;
                      Hashtbl.replace store k (Dv (mkvar k))) carried;
                residualize_while carried g b)
           end else if bstatic (bt_exp g) then
             (let n = ref 0 in
              while av_val (spec_exp g) <> VNil do
                incr n; if !n > 1000000 then failwith "static loop blew up"; spec b done)
           else residualize_while [] g b
       | _ -> failwith "cogen: bad command")
  | _ -> failwith "cogen: bad command"

(* ===== item-3 (M1a): the memoizing process-tree DRIVER (COGEN_MEMO) =====
 * Explicit continuation k (a list of pending commands).  `while` is unfolded into
 * the continuation, so re-reaching a driving point with the same (sigma_s, k) is a
 * back-edge -> we close a residual `while` (the subject's own loop), keeping
 * loop-invariant registers (the interpreter's code pointer) STATIC so the dispatch
 * inside the loop unrolls -> Jones-optimal (loop-free interpreter overhead). *)
let emit_asgn k v =                               (* like spec's asgn emit branch *)
  rc := l3 (a "'asgn") (mkvar k) (lift v) :: !rc;
  let rec selfref path = function
    | Cv (x, y) -> Cv (selfref (l2 (a "'hd") path) x, selfref (l2 (a "'tl") path) y)
    | Sv s -> Sv s | Dv _ -> Dv path in
  Hashtbl.replace store k (if !dyn_live = [] then selfref (mkvar k) v else Dv (mkvar k))

let rec drive (k : valT list) : unit =
  incr drive_steps; if !drive_steps > drive_budget then raise MemoBail;
  match k with
  | [] -> ()
  | c :: rest ->
    (match c with
     | VCons (VAtom (Atom "'asgn"), VCons (tgt, VCons (e, VNil))) ->
         let kk = depth (hd (tl tgt)) in
         let v = spec_exp e in
         if av_static v && not (List.mem kk !dyn_live) then Hashtbl.replace store kk v
         else emit_asgn kk v;
         drive rest
     | VCons (VAtom (Atom "'semi"), VCons (c1, VCons (c2, VNil))) ->
         drive (c1 :: c2 :: rest)
     | VCons (VAtom (Atom "'if"), VCons (g, VCons (c1, VCons (c2, VNil)))) ->
         (match spec_exp g with
          | Sv v -> drive ((if v <> VNil then c1 else c2) :: rest)
          | Cv _ -> drive (c1 :: rest)
          | Dv _ -> drive_dyn_if g c1 c2 rest)
     | VCons (VAtom (Atom "'while"), VCons (g, VCons (b, VNil))) ->
         (match spec_exp g with
          | Sv VNil -> drive rest
          | Sv _ | Cv _ -> drive (b :: c :: rest)      (* unfold one iteration *)
          | Dv _ ->                                    (* dynamic-guard while: treat as loop *)
              drive_dyn_if g (VCons (a "'semi", l2 b c)) (l3 (a "'asgn") (mkvar 1) (mkvar 1)) rest)
     | _ -> failwith "drive: bad command")

(* a dynamic-guard branch point: detect whether the THEN path folds back here (a
 * loop) and if so emit `while`, else emit a residual `if` (both branches driven). *)
and drive_dyn_if g c1 c2 rest =
  let key = (config (), rest) in
  if List.mem key !loop_stack then raise (Fold key);     (* back-edge *)
  let head = Hashtbl.copy store in
  (* dynamicize loop-carried (non-static) vars to residual refs; static vars stay
   * static so dispatch over them unrolls.  Body emits return them to (var k), so
   * the config recurs and the back-edge is detected. *)
  let carried = Hashtbl.fold (fun kk v acc -> if av_static v then acc else kk :: acc) store [] in
  List.iter (fun kk -> Hashtbl.replace store kk (Dv (mkvar kk))) carried;
  let gv = lift (spec_exp g) in
  let saved_rc = !rc and sdl = !dyn_live in
  loop_stack := key :: !loop_stack;
  dyn_live := carried;
  rc := [];
  let is_loop =
    (try drive (c1 :: rest); false with Fold kk when kk = key -> true) in
  loop_stack := (match !loop_stack with _ :: t -> t | [] -> []);
  let body = makeseq !rc in
  dyn_live := sdl;
  rc := saved_rc;
  if is_loop then begin
    Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) head;
    List.iter (fun kk -> Hashtbl.replace store kk (Dv (mkvar kk))) carried;
    rc := VCons (a "'while", l2 gv body) :: !rc;
    drive (c2 :: rest)                                   (* exit continuation *)
  end else begin
    (* genuine dynamic if: drive both branches to completion (continuation
     * duplicated -- correct, size cost), merging via the dynamicized carried set. *)
    let snap = Hashtbl.copy store in
    let s2 = !dyn_live in dyn_live := carried;
    rc := []; drive (c1 :: rest); let r1 = makeseq !rc in
    Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) snap;
    rc := []; drive (c2 :: rest); let r2 = makeseq !rc in
    dyn_live := s2;
    rc := VCons (a "'if", l3 gv r1 r2) :: saved_rc
  end

(* ===== item-3 M3: single-pass GLOBAL-memo driver (COGEN_M3) =====
 * The M2 driver (`drive`/`drive_dyn_if`, above) detects back-edges with an ANCESTOR
 * STACK + a `Fold` exception: it can only fold to the IMMEDIATE ancestor, cannot
 * share siblings, and a back-edge to a non-immediate ancestor escapes uncaught
 * (docs/ITEM3-M3-FINDINGS.md, findings #2-#4).  M3 replaces it with a GLOBAL memo
 * table `m3_seen : cfg -> name`: every driving point is looked up, a hit is a
 * back-edge to THAT block (any ancestor or sibling), and the residual is
 * reconstructed as structured WHILE (single back-edge dominating its body -> the
 * subject's own `while`; irreducible -> a pc-dispatch `while`, ITEM3-DESIGN.md
 * strategy (a)).  Non-loop dynamic-ifs emit a residual `if` with a real store merge,
 * driven under the SAME global memo so shared continuations are not re-driven.
 *
 * Built behind COGEN_M3 so the proven default and the M2 (COGEN_MEMO) paths stay
 * byte-identical.  M3a (this milestone): the worklist driver `drive2` + PC-dispatch
 * reconstruction below are LIVE and correctness-tested (examples/desugar/test-item3-m3.sh).
 * It is CORRECT on int_min/int_if/int_nest -- including int_if/int_nest, where the M2
 * trial prototype was wrong/blew up -- via the global memo + PC-dispatch (strategy a).
 * Subjects whose configuration set does not yet converge (usi*rev: the accumulator's
 * static spine grows every iteration with no whistle) hit the block budget and FALL
 * BACK to the proven `spec` (M3Bail), so correctness is never lost.  Still to come:
 * loop-free structured folding (M3b) and the homeomorphic-embedding whistle that lets
 * control-bearing loops converge (M3c). *)
let m3_on = (try Sys.getenv "COGEN_M3" <> "" with Not_found -> false)
let m3_seen : (cfg, int) Hashtbl.t = Hashtbl.create 64   (* config -> block name *)
let m3_name = ref 0
let m3_fresh () = incr m3_name; !m3_name
let m3_init () = Hashtbl.reset m3_seen; m3_name := 0

(* ===== item-3 M3a: the single-pass GLOBAL-memo worklist driver =====
 * Standard online recursive polyvariant specialization (Glück 2012 / JGS ch.4):
 * a CONFIGURATION is (program-point, static-store projection); a GLOBAL memo
 * `m3_seen : cfg -> name` is consulted at every driving point, so a re-reached
 * configuration is a back-edge to THAT block (any ancestor OR sibling) -- unlike
 * the M2 ancestor-stack/exception scheme (which folds only to the immediate
 * ancestor and crashes on non-immediate back-edges, docs/ITEM3-M3-FINDINGS.md).
 *
 * The residual is accumulated as labelled BLOCKS (a flowchart) and reconstructed
 * to a single core-WHILE command via strategy (a): a PC-dispatch loop
 *   PC := <entry>;  while PC do { if PC =? n1 then {body1; set-PC1} else ... }
 * (ITEM3-DESIGN.md).  This is ALWAYS correct and finite (it reintroduces one
 * dispatch loop -- loop-free structured folding is M3b, out of scope here).
 *
 * SAFETY NET: any unhandled situation raises `M3Bail`; run_spec catches it and
 * falls back to the proven `spec`, so correctness is NEVER lost. *)
exception M3Bail

type m3_transfer =
  | THalt
  | TGoto of int
  | TIf of valT * int * int        (* residual guard expr, then-name, else-name *)
type m3_block = { bname : int; mutable bstmts : valT list; mutable bxfer : m3_transfer }

(* block table + pending worklist (name, store-snapshot, continuation) *)
let m3_blocks : (int, m3_block) Hashtbl.t = Hashtbl.create 64
let m3_pending : (int * (int, av) Hashtbl.t * valT list) Queue.t = Queue.create ()
(* config-set finiteness backstop: a configuration that keeps growing (no whistle
 * yet -- that is M3c) would spawn unbounded blocks; cap and fall back to spec. *)
let m3_block_budget =
  try int_of_string (Sys.getenv "COGEN_M3_BUDGET") with _ -> 500
(* per-block straight-line drive guard (a static interpreter loop that spins at
 * spec time within ONE block); fall back to spec when exceeded. *)
let m3_step_budget =
  try int_of_string (Sys.getenv "COGEN_M3_STEPS") with _ -> 200000

(* PC variable index: a guaranteed-unused slot.  run_spec scans the program for the
 * largest var index actually used and sets this to (max+1), so it cannot collide
 * with any program variable.  Kept small (block names are nil^n; a small PC index
 * keeps the dispatch numerals compact). *)
let m3_pc = ref 100000
let rec max_var_e e = match e with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'var", VCons (k, VNil) -> depth k
       | "'quote", _ -> 0
       | "'hd", VCons (e1, VNil) | "'tl", VCons (e1, VNil) -> max_var_e e1
       | "'cons", VCons (e1, VCons (e2, VNil))
       | "'eq", VCons (e1, VCons (e2, VNil)) -> max (max_var_e e1) (max_var_e e2)
       | _ -> 0)
  | _ -> 0
let rec max_var_c c = match c with
  | VCons (VAtom (Atom op), args) ->
      (match op, args with
       | "'asgn", VCons (tgt, VCons (e, VNil)) -> max (max_var_e tgt) (max_var_e e)
       | "'semi", VCons (c1, VCons (c2, VNil)) -> max (max_var_c c1) (max_var_c c2)
       | "'while", VCons (g, VCons (b, VNil)) -> max (max_var_e g) (max_var_c b)
       | "'if", VCons (g, VCons (c1, VCons (c2, VNil))) ->
           max (max_var_e g) (max (max_var_c c1) (max_var_c c2))
       | _ -> 0)
  | _ -> 0

(* the static projection of a store SNAPSHOT, computed by temporarily installing
 * it into the global `store` (save/restore around `config ()`). *)
let proj_snapshot (snap : (int, av) Hashtbl.t) : (int * valT) list =
  let saved = Hashtbl.copy store in
  Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) snap;
  let p = config () in
  Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) saved;
  p

(* specialize a configuration (snapshot, k): return its block name, enqueueing if
 * it is new.  The memo key is (static projection of snapshot, k). *)
let m3_specialize (snap : (int, av) Hashtbl.t) (k : valT list) : int =
  let key = (proj_snapshot snap, k) in
  match Hashtbl.find_opt m3_seen key with
  | Some n -> n
  | None ->
      let n = m3_fresh () in
      if n > m3_block_budget then raise M3Bail;   (* config set not converging *)
      Hashtbl.replace m3_seen key n;
      Queue.add (n, Hashtbl.copy snap, k) m3_pending;
      n

(* numeral VALUE used to tag block name n (>=1) and as PC content; halt = VNil.
 * We reuse mkvar n = ('var . nil^n) purely as a distinct data value. *)
let m3_pcval n = mkvar n
(* PC =? <numeral n> as a residual guard expression *)
let m3_pc_is n = l3 (a "'eq") (mkvar !m3_pc) (l2 (a "'quote") (m3_pcval n))
(* set-PC command for a transfer *)
let m3_setpc = function
  | THalt -> l3 (a "'asgn") (mkvar !m3_pc) (l2 (a "'quote") VNil)
  | TGoto n -> l3 (a "'asgn") (mkvar !m3_pc) (l2 (a "'quote") (m3_pcval n))
  | TIf (g, nt, ne) ->
      VCons (a "'if", l3 g
               (l3 (a "'asgn") (mkvar !m3_pc) (l2 (a "'quote") (m3_pcval nt)))
               (l3 (a "'asgn") (mkvar !m3_pc) (l2 (a "'quote") (m3_pcval ne))))

(* drive one block: restore its snapshot, run straight-line + STATIC control
 * inline (mirroring the M2 `drive` asgn/semi/static-if/static-while cases),
 * appending residual to `rc`, until a DYNAMIC-control point or []. *)
let m3_run_block name (snap : (int, av) Hashtbl.t) (k0 : valT list) : unit =
  Hashtbl.reset store; Hashtbl.iter (Hashtbl.replace store) snap;
  rc := [];
  dyn_live := [];
  let steps = ref 0 in
  let rec go k =
    incr steps; if !steps > m3_step_budget then raise M3Bail;
    match k with
    | [] -> THalt
    | c :: rest ->
      (match c with
       | VCons (VAtom (Atom "'asgn"), VCons (tgt, VCons (e, VNil))) ->
           let kk = depth (hd (tl tgt)) in
           let v = spec_exp e in
           if av_static v && not (List.mem kk !dyn_live) then Hashtbl.replace store kk v
           else emit_asgn kk v;
           go rest
       | VCons (VAtom (Atom "'semi"), VCons (c1, VCons (c2, VNil))) ->
           go (c1 :: c2 :: rest)
       | VCons (VAtom (Atom "'if"), VCons (g, VCons (c1, VCons (c2, VNil)))) ->
           (match spec_exp g with
            | Sv v -> go ((if v <> VNil then c1 else c2) :: rest)
            | Cv _ -> go (c1 :: rest)
            | Dv _ ->
                (* dynamic if: dynamicize the escaping (non-static assigned) vars in
                 * the CURRENT store so both successors can later coincide, then
                 * specialize each successor under the SAME global memo. *)
                let esc = uni (List.filter (fun x -> not (bstatic (bget x))) (assigned_vars c1))
                              (List.filter (fun x -> not (bstatic (bget x))) (assigned_vars c2)) in
                List.iter (fun x -> Hashtbl.replace store x (Dv (mkvar x))) esc;
                let gv = lift (spec_exp g) in
                let snap2 = Hashtbl.copy store in
                let nt = m3_specialize snap2 (c1 :: rest) in
                let ne = m3_specialize snap2 (c2 :: rest) in
                TIf (gv, nt, ne))
       | VCons (VAtom (Atom "'while"), VCons (g, VCons (b, VNil))) ->
           (match spec_exp g with
            | Sv VNil -> go rest
            | Sv _ | Cv _ -> go (b :: c :: rest)             (* unfold one iteration *)
            | Dv _ ->
                (* dynamic while = if g then (b; while g b; rest) else rest *)
                let esc = List.filter (fun x -> not (bstatic (bget x))) (assigned_vars b) in
                List.iter (fun x -> Hashtbl.replace store x (Dv (mkvar x))) esc;
                let gv = lift (spec_exp g) in
                let snap2 = Hashtbl.copy store in
                let nt = m3_specialize snap2 (b :: c :: rest) in
                let ne = m3_specialize snap2 rest in
                TIf (gv, nt, ne))
       | _ -> raise M3Bail)
  in
  let xfer = go k0 in
  (* if this block halts and the output var is still static, materialize it so the
   * residual writes it (mirrors run_spec's epilogue reconstruction). *)
  (match xfer with
   | THalt when !out_var >= 0 && (match sget !out_var with Dv _ -> false | _ -> true) ->
       emit_asgn !out_var (sget !out_var)
   | _ -> ());
  Hashtbl.replace m3_blocks name { bname = name; bstmts = !rc; bxfer = xfer }

(* M3a entry point: build the block graph by worklist to fixpoint, then emit a
 * PC-dispatch loop into `rc`.  Raises M3Bail on any unhandled situation. *)
let drive2 (body : valT) : unit =
  Hashtbl.reset m3_blocks; Queue.clear m3_pending;
  let init_snap = Hashtbl.copy store in
  let entry = m3_specialize init_snap [body] in
  while not (Queue.is_empty m3_pending) do
    let (name, snap, k) = Queue.pop m3_pending in
    m3_run_block name snap k
  done;
  (* reconstruct: PC := entry; while PC do <dispatch>.  Dispatch is a right-nested
   * if-chain over the block names, in ascending name order (deterministic). *)
  let names = List.sort compare (Hashtbl.fold (fun n _ acc -> n :: acc) m3_blocks []) in
  let dispatch =
    List.fold_right
      (fun n acc ->
        let blk = Hashtbl.find m3_blocks n in
        (* block body = bstmts (reversed accumulator) then set-PC, as a 'semi seq *)
        let body_seq = makeseq (m3_setpc blk.bxfer :: blk.bstmts) in
        VCons (a "'if", l3 (m3_pc_is n) body_seq acc))
      names
      (l3 (a "'asgn") (mkvar 1) (mkvar 1))   (* harmless else-skip: (var 1):=(var 1) *)
  in
  let pc_init = l3 (a "'asgn") (mkvar !m3_pc) (l2 (a "'quote") (m3_pcval entry)) in
  let loop = VCons (a "'while", l2 (mkvar !m3_pc) dispatch) in
  (* rc holds the residual reversed (makeseq folds it); [loop; pc_init] => pc_init;loop *)
  rc := [loop; pc_init];
  (* force run_spec to emit makeseq !rc verbatim: the blocks already wrote out_var *)
  if !out_var >= 0 then Hashtbl.replace store !out_var (Dv (mkvar !out_var))

let run_spec progpath spath =
  let prog = let ch = open_in progpath in let v = parseValT ch in close_in ch; v in
  let s    = let ch = open_in spath    in let v = parseValT ch in close_in ch; v in
  let varI = hd prog and body = hd (tl prog) and varJ = hd (tl (tl prog)) in
  let i = depth (hd (tl varI)) and j = depth (hd (tl varJ)) in
  Hashtbl.replace benv i (C (S, D));
  changed := true; while !changed do changed := false; bpass false body done;
  (* input In = (s . d): head s static, tail d = the residual's input var (var i) *)
  let init () = Hashtbl.reset store; Hashtbl.replace store i (Cv (Sv s, Dv (mkvar i)));
                rc := []; cont_stack := []; loop_stack := []; dyn_live := [] in
  init ();
  out_var := j;
  if !online && memo_on then
    (drive_steps := 0;
     try drive [body]
     with MemoBail -> init (); spec body)   (* fall back to the non-memo specializer *)
  else if !online && m3_on then
    (m3_init ();
     m3_pc := max_var_c body + 1;   (* guaranteed-unused PC slot (scanned the prog) *)
     try drive2 body
     with _ -> init (); spec body)          (* SAFETY NET: never lose correctness *)
  else spec body;
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
