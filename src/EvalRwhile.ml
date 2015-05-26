open AbsRwhile
open PrintRwhile
open List

type store = (rIdent * valT) list

let vtrue = VCons (VNil, VNil)
let vfalse = VNil

let prtStore (i : int) (e : (rIdent * valT) list) : doc = 
  let rec f = function
    | [] -> concatD []
    | [(x,v)] -> concatD [prtRIdent 0 x; render ":="; prtValT 0 v]
    | (x,v) :: ss -> concatD [prtRIdent 0 x; render ":="; prtValT 0 v; render "," ; f ss]
  in concatD [render "{"; f e; render "}"]

(* リストに要素を追加する。ただし、すでにその要素がリストにある場合は追加しない。 *)
let rec insert x = function 
  | [] -> [x]
  | y :: ys -> y :: if x = y then ys else insert x ys

let merge xs ys = fold_right insert xs ys

(* Reversible update *)
let rec rupdate (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtRIdent x ^ " is not found (1)")
  | (y, vy) :: ys -> if x = y 
		     then (if vy = VNil 
			   then (y, vx)
			   else if vx = vy 
			   then (y, VNil)
			   else if vx = VNil
			   then (y, vy)
			   else failwith "error in update") :: ys
		     else (y, vy) :: rupdate (x, vx) ys

(* Irreversible update *)
let rec update (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtRIdent x ^ " is not found (2)")
  | (y, vy) :: ys -> if x = y
		     then (y, vx) :: ys
		     else (y, vy) :: update (x, vx) ys

let all_cleared (s : store) = for_all (fun (_, v) -> v = VNil) s

(* プログラム中に使用されている変数名を列挙する。 *)
let rec varProgram (Prog (ms, x, c, y)) = insert x (insert y (varCom c))

and varMac (Mac (_,xs,c)) = merge xs (varCom c)

and varCom = function
  | CMac (_, xs) -> merge xs []
  | CAss (x,e) -> insert x (varExp e)
  | CRep (q, r) -> merge (varPat q) (varPat r)
  | CSeq (c, d) -> merge (varCom c) (varCom d)
  | CCond (e, thenBranch, elseBranch, f) -> 
     fold_right merge [varExp e; varExp f; varThenBranch thenBranch; varElseBranch elseBranch] []
  | CLoop (e, doBranch, loopBranch, f) ->
     fold_right merge [varExp e; varExp f; varDoBranch doBranch; varLoopBranch loopBranch] []
  | CShow _ -> []

and varPat : pat -> rIdent list = function
   PCons (q,r) -> merge (varPat q) (varPat r)
 | PVar (Var x) -> [x]
 | PVal _ -> []

and varThenBranch = function
  | BThen com -> varCom com
  | BThenNone -> []

and varElseBranch = function
  | BElse com -> varCom com
  | BElseNone -> []

and varDoBranch = function
  | BDo com -> varCom com
  | BDoNone -> []

and varLoopBranch = function
  | BLoop com -> varCom com
  | BLoopNone -> []

and varExp : exp -> rIdent list = function
  | ECons (e1, e2) -> merge (varExp e1) (varExp e2)
  | EHd e -> varExp e
  | ETl e -> varExp e
  | EEq (e1, e2) -> merge (varExp e1) (varExp e2)
  | EVar (Var x) -> [x]
  | EVal v -> []

(* Evaluation *)
let evalVariable s (Var x) = assoc x s

let rec evalExp s = function
    ECons (e1, e2) -> VCons (evalExp s e1, evalExp s e2)
  | EHd e -> (match evalExp s e with
	      | VNil | VAtom _ as v -> failwith ("No head. Expression " ^ printTree prtExp (EHd e) ^ " has value " ^ printTree prtValT v)
	      | VCons (v,_) -> v)
  | ETl e -> (match evalExp s e with
	      | VNil | VAtom _ as v -> failwith ("No tail. Expression " ^ printTree prtExp (ETl e) ^ " has value " ^ printTree prtValT v)
	      | VCons (_,v) -> v)
  | EEq (e1, e2) -> if evalExp s e1 = evalExp s e2 then vtrue else vfalse
  | EVar x -> evalVariable s x
  | EVal v -> v

and evalPat s = function
    PCons (q, r) -> let (s1, d1) = evalPat s q in
		      let (s2, d2) = evalPat s1 r in
		      (s2, VCons (d1, d2))
  | PVar (Var y) -> let v = evalVariable s (Var y) in 
		    (update (y,VNil) s, v)
  | PVal val' -> (s, val')

and inv_evalPat s = function
    (PCons (p1, p2), VCons (v1, v2)) -> let s1 = inv_evalPat s (p1, v1) in
					inv_evalPat s1 (p2, v2)
  | (PVar (Var y) as p, v) -> if evalVariable s (Var y) = VNil
			      then update (y, v) s
			      else (print_string ("impossible happened in inv_evalPat.PVar\n" ^ 
						    "pattern: " ^ printTree prtPat p ^ "\n" ^
						      "term: " ^ printTree prtValT v ^ "\n" ^
							"store: " ^ printTree prtStore s ^ "\n");
				    failwith "in inv_evalPat.PVar"
			    )
  | (PVal v', v) -> if v = v' then s
		    else failwith ("Pattern matching failed.\n" ^ 
				     printTree prtValT v ^ " and " ^ printTree prtValT v' ^ " are not equal (in inv_evalPat)\n" ^
				       printTree prtStore s ^ "\n")
  | (PCons _ as p, v) -> failwith ("impossible happened in inv_evalPat.PCons\n" ^ 
				     "pattern: " ^ printTree prtPat p ^ "\n" ^
				       "term: " ^ printTree prtValT v ^ "\n" ^
					 "store: " ^ printTree prtStore s ^ "\n"
				  )

and evalCom (s : store) : com -> store = function
  | CMac (_, _) -> failwith "Impossible happened.  Macro must not appear in runtime."
  | CAss (y, e) -> let v' = evalExp s e in
		   rupdate (y, v') s
  | CRep (q, r) -> let (s1, v1) = evalPat s r in
		   inv_evalPat s1 (q, v1)
  | CSeq (c, d) -> let s1 = evalCom s c in
		   evalCom s1 d
  | CCond (e, thenbranch, elsebranch, f) -> 
     if evalExp s e = vtrue then
       let s1 = (match thenbranch with
		 | BThen c -> evalCom s c
		 | BThenNone -> s) 
       in
       if evalExp s1 f = vtrue then s1
       else failwith ("Assertion " ^ printTree prtExp f ^ " is not true.\n")
     else
       let s1 = match elsebranch with
	 | BElse c -> evalCom s c
	 | BElseNone -> s
       in
       if evalExp s1 f = vfalse then s1 
       else failwith ("Assertion " ^ printTree prtExp f ^ " is not false.\n")
  | CLoop (e, dobranch, loopbranch, f) ->
     if evalExp s e = vtrue then
       let s1 = match dobranch with
	   BDo c -> evalCom s c
	 | BDoNone -> s
       in
       evalLoop s1 (e, dobranch, loopbranch, f)
     else failwith ("Assertion " ^ printTree prtExp e ^ " is not true.\n")
  | CShow e -> (print_string (printTree prtExp e ^ " = " ^ printTree prtValT (evalExp s e) ^ "\n"); s)

and evalLoop (s : store) (e, dobranch, loopbranch, f) : store =
  if evalExp s f = VCons (VNil, VNil)
  then s
  else
    let s1 = (match loopbranch with
	 	BLoop c   -> evalCom s c
	      | BLoopNone -> s)
    in
    assert (evalExp s1 e = VNil);
    let s2 = (match dobranch with
		BDo c   -> evalCom s1 c
	      | BDoNone -> s1)
    in
    evalLoop s2 (e, dobranch, loopbranch, f)

and evalProgram (p : program) (v : valT): valT =
  let Prog (ms, x, c, y) as p' = MacroRwhile.expMacProgram p in
  let s = map (fun x -> (x, VNil)) (varProgram p') in
  let s1 = rupdate (x, v) s in
  let s2 = evalCom s1 c in
  let res = evalVariable s2 (Var y) in
  let s3 = rupdate (y, res) s2 in
  if all_cleared s3 then res 
  else failwith "Some variables are not nil."
