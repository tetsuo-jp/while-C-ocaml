open AbsWhile
open PrintWhile
open List

type store = (ident * valT) list

(* リストに要素を追加する。ただし、すでにその要素がリストにある場合は追加しない。 *)
let rec insert x = function 
  | [] -> [x]
  | y :: ys -> y :: if x = y then ys else insert x ys

let merge xs ys = fold_right insert xs ys

(* Update stores *)
let rec update (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtIdent x ^ " is not found (2)")
  | (y, vy) :: ys -> if x = y
		     then (y, vx) :: ys
		     else (y, vy) :: update (x, vx) ys

(* プロシージャ中に使用されている変数名を列挙する。 *)
let rec varProc (AProc (x,cs,y) : proc) : ident list =
  insert x (insert y (fold_right insert (concat (map varCom cs)) []))

and varCom : com -> ident list = function
  | CAsn (x,e) -> insert x (varExp e)
  | CLoop (e, cs) ->
     fold_right merge [varExp e; concat (map varCom cs)] []

and varExp : exp -> ident list = function
  | ECons (e1, e2) -> merge (varExp e1) (varExp e2)
  | EHd e -> varExp e
  | ETl e -> varExp e
  | EEq (e1, e2) -> merge (varExp e1) (varExp e2)
  | EVar x -> [x]
  | EVal v -> []

(* Evaluation *)
let evalVariable s x = assoc x s

let rec evalExp s = function
    ECons (e1, e2) -> VCons (evalExp s e1, evalExp s e2)
  | EHd e -> (match evalExp s e with
	      | VNil | VAtom _ as v -> failwith ("No head. Expression " ^ printTree prtExp (EHd e) ^ " has value " ^ printTree prtValT v)
	      | VCons (v,_) -> v)
  | ETl e -> (match evalExp s e with
	      | VNil | VAtom _ as v -> failwith ("No tail. Expression " ^ printTree prtExp (ETl e) ^ " has value " ^ printTree prtValT v)
	      | VCons (_,v) -> v)
  | EEq (e1, e2) -> if evalExp s e1 = evalExp s e2 then VCons (VNil, VNil) else VNil
  | EVar x -> evalVariable s x
  | EVal v -> v

let rec evalCom (s : store) : com -> store = function
  | CAsn (y, e) -> update (y, evalExp s e) s
  | CLoop (e, cs) ->
     if evalExp s e = VNil then
       s
     else
       let s' = evalComs s cs
       in evalCom s' (CLoop (e, cs))

and evalComs (s : store) (cs : com list) : store = fold_left evalCom s cs

let evalProc (AProc (x,cs,y) as p : proc) (v : valT) : valT = 
  let s0 = map (fun x -> (x, VNil)) (varProc p) in
  let s1 = update (x, v) s0 in
  let s2 = evalComs s1 cs in
  evalVariable s2 y

  

