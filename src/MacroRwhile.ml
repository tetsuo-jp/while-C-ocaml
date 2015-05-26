(* Macro Expansion *)

open PrintRwhile
open AbsRwhile
open InvRwhile
open List

(* Substitution *)
type subst = (rIdent * rIdent) list

let rec substRIdent (ss : subst) (x : rIdent) : rIdent = try assoc x ss with Not_found -> x

and substVariable ss (Var x) = Var (substRIdent ss x)

and substExp ss = function
    ECons (e, f) -> ECons (substExp ss e, substExp ss f)
  | EHd e -> EHd (substExp ss e)
  | ETl e -> ETl (substExp ss e)
  | EEq (e, f) -> EEq (substExp ss e, substExp ss f)
  | EVar x -> EVar (substVariable ss x)
  | EVal v -> EVal v

and substPat ss = function
    PCons (q, r) -> PCons (substPat ss q, substPat ss r)
  | PVar x -> PVar (substVariable ss x)
  | PVal v -> PVal v

and substCom ss = function
    CMac (m, xs) -> let xs' = map (fun y -> try assoc y ss with Not_found -> y) xs
		    in CMac (m, xs')
  | CAss (x, e) -> CAss (substRIdent ss x, substExp ss e)
  | CRep (q, r) -> CRep (substPat ss q, substPat ss r)
  | CSeq (c, d) -> CSeq (substCom ss c, substCom ss d)
  | CCond (e, thenbranch, elsebranch, f) ->
     CCond (substExp ss e, substThenBranch ss thenbranch, substElseBranch ss elsebranch, substExp ss f)
  | CLoop (e, dobranch, loopbranch, f) ->
     CLoop (substExp ss e, substDoBranch ss dobranch, substLoopBranch ss loopbranch, substExp ss f)
  | CShow e -> CShow (substExp ss e)

and substThenBranch ss = function
    BThen com -> BThen (substCom ss com)
  | BThenNone -> BThenNone

and substElseBranch ss = function
    BElse com -> BElse (substCom ss com)
  | BElseNone -> BElseNone

and substDoBranch s = function
    BDo c   -> BDo (substCom s c)
  | BDoNone -> BDoNone

and substLoopBranch s = function
    BLoop c   -> BLoop (substCom s c)
  | BLoopNone -> BLoopNone


(* Macro expansion *)
let rec expMacCom (ms : macro list) = function
    CMac (m, xs) -> let rec expandMacro = function
		      | [] -> failwith ("Macro " ^ printTree prtRIdent m ^ " not found")
		      | Mac (m',xs',c) :: ns -> try if invMacroName m' = m
						    then expMacCom ms (substCom (combine xs' xs) (invCom c))
						    else if m = m' 
						    then expMacCom ms (substCom (combine xs' xs) c) 
						    else expandMacro ns
						with Invalid_argument "List.combine" ->
						  failwith ("Macro " ^ printTree prtRIdent m ^ " expects " ^ string_of_int (length xs') ^ " argument(s) but got " ^ string_of_int (length xs))
		    in expandMacro ms
  | CAss _ as e -> e
  | CRep _ as e -> e
  | CSeq (c, d) -> CSeq (expMacCom ms c, expMacCom ms d)
  | CCond (e, thenbranch, elsebranch, f) -> 
     CCond (e, expMacThenBranch ms thenbranch, expMacElseBranch ms elsebranch, f)
  | CLoop (e, dobranch, loopbranch, f) -> 
     CLoop (e, expMacDoBranch ms dobranch, expMacLoopBranch ms loopbranch, f)
  | CShow e -> CShow e

and expMacThenBranch ms = function
    BThen c   -> BThen (expMacCom ms c)
  | BThenNone -> BThenNone

and expMacElseBranch ms = function
    BElse c   -> BElse (expMacCom ms c)
  | BElseNone -> BElseNone

and expMacDoBranch ms = function
    BDo c   -> BDo (expMacCom ms c)
  | BDoNone -> BDoNone

and expMacLoopBranch ms = function
    BLoop c   -> BLoop (expMacCom ms c)
  | BLoopNone -> BLoopNone

and expMacProgram (Prog (ms, x, c, y)) = Prog ([], x, expMacCom ms c, y)
