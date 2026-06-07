open AbsWhile
open PrintWhile
open List

type store = (id * valT) list

(* Update stores *)
let rec update (x, vx) = function
  | [] -> failwith ("Variable " ^ printTree prtId x ^ " is not found")
  | (y, vy) :: ys -> if x = y
		     then (y, vx) :: ys
		     else (y, vy) :: update (x, vx) ys

let rec varExp : exp -> id list = function
  | ECons (e1, e2) -> varExp e1 @ varExp e2
  | EHd e -> varExp e
  | ETl e -> varExp e
  | EEq (e1, e2) -> varExp e1 @ varExp e2
  | EVar x -> [x]
  | EVal v -> []

let rec varCom : com -> id list = function
  | CAsn (x,e) -> x :: varExp e
  | CLoop (e, cs) ->
     varExp e @ concat (map varCom cs)

(* プロシージャ中に使用されている変数名を列挙する。重複は取り除く。 *)
let rec varProc (AProc (x,cs,y) : proc) : id list =
  sort_uniq Stdlib.compare (x :: y :: concat (map varCom cs))

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

(* Timing: unit cost per operation/test on data *)
(* Definition "Running times of WHILE programs" (measuring-time, ch.16-19) *)

(* 式の評価時間 T。値に依存せず、式の構造のみで決まる。
   =? は教科書では nil 比較に展開される前提だが、ここでは単位コストとする。 *)
let rec timeExp : exp -> int = function
  | ECons (e1, e2) -> 1 + timeExp e1 + timeExp e2
  | EHd e -> 1 + timeExp e
  | ETl e -> 1 + timeExp e
  | EEq (e1, e2) -> 1 + timeExp e1 + timeExp e2
  | EVar _ -> 1
  | EVal _ -> 1

(* コマンドの実行時間 ⊢time。実行しつつ経過時間を蓄積する (末尾再帰)。
     X := E        : T[E] + 1
     while (nil)   : T[E] + 1
     while (非 nil): T[E] + 1 + time(C) + time(while E do C)  *)
let rec timeCom (s : store) (acc : int) : com -> store * int = function
  | CAsn (y, e) -> (update (y, evalExp s e) s, acc + timeExp e + 1)
  | CLoop (e, cs) as loop ->
     if evalExp s e = VNil then
       (s, acc + timeExp e + 1)
     else
       let (s', acc') = timeComs s (acc + timeExp e + 1) cs in
       timeCom s' acc' loop

and timeComs (s : store) (acc : int) (cs : com list) : store * int =
  fold_left (fun (s, acc) c -> timeCom s acc c) (s, acc) cs

(* time_p(d): 結果と実行ステップ数の対を返す *)
let timeProc (AProc (x,cs,y) as p : proc) (v : valT) : valT * int =
  let s0 = map (fun x -> (x, VNil)) (varProc p) in
  let s1 = update (x, v) s0 in
  let (s2, t) = timeComs s1 0 cs in
  (assoc y s2, t)
