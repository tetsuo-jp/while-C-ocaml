open AbsWhile
open PrintWhile
open List

type store = (ident * valT) list

(* Update stores *)
let rec update (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtIdent x ^ " is not found")
  | (y, vy) :: ys -> if x = y
		     then (y, vx) :: ys
		     else (y, vy) :: update (x, vx) ys

let rec varExp : exp -> ident list = function
  | ECons (e1, e2) -> varExp e1 @ varExp e2
  | EHd e -> varExp e
  | ETl e -> varExp e
  | EEq (e1, e2) -> varExp e1 @ varExp e2
  | EVar x -> [x]
  | EVal v -> []

let rec varCom : com -> ident list = function
  | CAsn (x,e) -> x :: varExp e
  | CLoop (e, cs) ->
     varExp e @ concat (map varCom cs)

(* プロシージャ中に使用されている変数名を列挙する。重複は取り除く。 *)
let rec varProc (AProc (x,cs,y) : proc) : ident list =
  sort_uniq compare (x :: y :: concat (map varCom cs))

(* Evaluation of expressions *)
(* p.37 Definition 2.2.2 *)
let rec evalExp s = function
    ECons (e1, e2) -> VCons (evalExp s e1, evalExp s e2)
  | EHd e -> (match evalExp s e with
	      | VNil | VAtom _ -> VNil
	      | VCons (v,_) -> v)
  | ETl e -> (match evalExp s e with
	      | VNil | VAtom _ -> VNil
	      | VCons (_,v) -> v)
  | EEq (e1, e2) -> if evalExp s e1 = evalExp s e2 then VCons (VNil, VNil) else VNil
  | EVar x -> assoc x s
  | EVal v -> v

(* Execution of commands *)
(* p.38 Definition 2.2.3 *)
let rec evalCom (s : store) : com -> store = function
  | CAsn (y, e) -> update (y, evalExp s e) s
  | CLoop (e, cs) ->
     if evalExp s e = VNil then
       s
     else
       let s' = evalComs s cs
       in evalCom s' (CLoop (e, cs))

and evalComs (s : store) (cs : com list) : store = fold_left evalCom s cs

(* Semantics of WHILE programs *)
(* p.38 Definition 2.2.4 *)
let evalProc (AProc (x,cs,y) as p : proc) (v : valT) : valT = 
  let s0 = map (fun x -> (x, VNil)) (varProc p) in
  let s1 = update (x, v) s0 in
  let s2 = evalComs s1 cs in
  assoc y s2
