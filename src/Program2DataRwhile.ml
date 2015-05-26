open AbsRwhile
open List

let atom str = VAtom (Atom str)

let rec conss = function	(* 演算子 cons* *)
  | [] -> failwith "error in conss"
  | [v] -> v
  | v :: vs -> VCons (v, conss vs)

let rec repeat s n = if n = 0 then [] else s :: repeat s (n-1)

let doNothing = conss [atom "'ass"; conss [atom "'var"; VNil]; conss [atom "'val"; VNil]]

let rec transRIdent (RIdent str) : valT =
  try conss (repeat VNil (int_of_string str))
  with Failure("int_of_string") -> failwith "Impossible happened."

and transVariable (Var rident) : valT = conss [atom "'var"; transRIdent rident]

and transExp (x : exp) : valT = 
  conss (match x with
	 | EVar var -> [transVariable var]
	 | EVal val' -> [atom "'val"; val']
	 | ECons (e1, e2) -> [atom "'cons"; transExp e1; transExp e2]
	 | EHd e -> [atom "'hd"; transExp e]
	 | ETl e -> [atom "'tl"; transExp e]
	 | EEq (e1, e2) -> [atom "'eq"; transExp e1; transExp e2])

and transPat (x : pat) : valT = 
  conss (match x with
	 | PCons (p1, p2) -> [atom "'cons"; transPat p1; transPat p2]
	 | PVar var -> [transVariable var]
	 | PVal val' -> [atom "'val"; val'])

and transCom (c : com) : valT = 
  conss (match c with
	 | CSeq (c1, c2) -> [atom "'seq"; transCom c1; transCom c2]
	 | CMac (rident, ridents) -> failwith "error in transCom"
	 | CAss (x, e) -> [atom "'ass"; conss [atom "'var"; transRIdent x]; transExp e]
	 | CRep (p1, p2) -> [atom "'rep"; transPat p1; transPat p2]
	 | CCond (e, thenbranch, elsebranch, f) -> 
	    [atom "'cond"; conss [transExp e; transThenBranch thenbranch; transElseBranch elsebranch; transExp f; VNil]]
	 | CLoop (e, dobranch, loopbranch, f) -> 
	    [atom "'loop"; conss [transExp e; transDoBranch dobranch; transLoopBranch loopbranch; transExp f; VNil]]
	 | CShow e -> [])

and transThenBranch = function
    BThen com -> transCom com
  | BThenNone -> doNothing

and transElseBranch = function
    BElse com -> transCom com
  | BElseNone -> doNothing

and transDoBranch = function
    BDo com -> transCom com
  | BDoNone -> doNothing

and transLoopBranch = function
    BLoop com -> transCom com
  | BLoopNone -> doNothing

and transProgram = function
    Prog ([], x, c, y) -> conss [conss [atom "'var"; transRIdent x];
				transCom c;
				conss [atom "'var"; transRIdent y]]
  | _ -> failwith "Impossible happened."

and transMacro (x : macro) : valT = match x with
    Mac (rident, ridents, com) -> failwith "error in transMacro"


(* substitution *)
type subst = (rIdent * rIdent) list

let rec substRIdent (s : subst) (x : rIdent) : rIdent = 
  try assoc x s with Not_found -> x

and substVariable s (Var x) : variable = Var (substRIdent s x)

and substExp s = function
    ECons (e, f) -> ECons (substExp s e, substExp s f)
  | EHd e -> EHd (substExp s e)
  | ETl e -> ETl (substExp s e)
  | EEq (e, f) -> EEq (substExp s e, substExp s f)
  | EVar x -> EVar (substVariable s x)
  | EVal v -> EVal v

and substPat s = function
    PCons (p1, p2) -> PCons (substPat s p1, substPat s p2)
  | PVar x -> PVar (substVariable s x)
  | PVal v -> PVal v

and substCom s = function
    CSeq (c, d) -> CSeq (substCom s c, substCom s d)
  | CMac (m, xs) -> CMac (m, map (substRIdent s) xs)
  | CAss (x, e) -> CAss (substRIdent s x, substExp s e)
  | CRep (p1, p2) -> CRep (substPat s p1, substPat s p2)
  | CCond (e, thenbranch, elsebranch, f) ->
     CCond (substExp s e, substThenBranch s thenbranch, substElseBranch s elsebranch, substExp s f)
  | CLoop (e, dobranch, loopbranch, f) ->
     CLoop (substExp s e, substDoBranch s dobranch, substLoopBranch s loopbranch, substExp s f)
  | CShow e -> CShow (substExp s e)

and substThenBranch s = function
    BThen c   -> BThen (substCom s c)
  | BThenNone -> BThenNone

and substElseBranch s = function
    BElse c   -> BElse (substCom s c)
  | BElseNone -> BElseNone

and substDoBranch s = function
    BDo c   -> BDo (substCom s c)
  | BDoNone -> BDoNone

and substLoopBranch s = function
    BLoop com -> BLoop (substCom s com)
  | BLoopNone -> BLoopNone

and substProgram s (Prog (macros, x, c, y)) =
  Prog (map (substMacro s) macros, substRIdent s x, substCom s c, substRIdent s y)

and substMacro s (Mac (x, xs, c)) = Mac (x, map (substRIdent s) xs, substCom s c)

let program2data (p : program) : valT =
  let p2 = MacroRwhile.expMacProgram p in
  let vs = EvalRwhile.varProgram p2 in
  let rec incseq m n = if m = n then [m] else m :: incseq (m+1) n in
  let ws = map (fun n -> RIdent (string_of_int n)) (incseq 1 (length vs)) in
  let p3 = substProgram (combine vs ws) p2 in
  (* print_string (PrintRwhile.printTree PrintRwhile.prtProgram p3 ^ "\n"); *)
  transProgram p3
