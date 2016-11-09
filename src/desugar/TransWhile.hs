module TransWhile where

import AbsWhile

class Trans a where
  trans :: a -> a

instance Trans Ident where
  trans x = case x of
    Ident string -> Ident string

instance Trans Atom where
  trans x = case x of
    Atom string -> Atom string

instance Trans Program where
  trans x = case x of
    Prog procs -> Prog (map trans procs)

instance Trans Proc where
  trans x = case x of
    AProc pnameop ident1 coms ident2 ->
      AProc pnameop ident1 (map trans coms) ident2

instance Trans PNameOp where
  trans x = case x of
    Name ident -> Name ident
    NoName -> NoName

instance Trans Com where
  trans x = case x of
    CAsn ident exp -> CAsn ident (trans exp)
    CProc ident1 ident2 ident3 -> CProc ident1 ident2 ident3
    CLoop exp coms -> CLoop (trans exp) (map trans coms)
    CShow exp -> CShow (trans exp)

instance Trans Exp where
  trans x = case x of
    ECons exp1 exp2 -> ECons (trans exp1) (trans exp2)
    EConsp ident -> let x = EVar ident in EEq x (ECons (EHd x) (ETl x))
    EAtomp ident -> EEq (trans (EVal VFalse)) (trans (EConsp ident))
    EHd exp -> EHd (trans exp)
    ETl exp -> ETl (trans exp)
    EEq exp1 exp2 -> EEq (trans exp1) (trans exp2)
    EListRep exps tl -> case tl of 
      NoTail -> foldr ECons (EVal VNil) (map trans exps)
      Tail atom -> foldr ECons (EVal (VAtom (trans atom))) (map trans exps)
    EVar ident -> EVar ident
    EVal val -> EVal (trans val)
    EConsStar exps -> foldr1 ECons $ map trans exps
    EList exps -> foldr ECons (EVal VNil) $ map trans exps

instance Trans Val where
  trans x = case x of
    VNil -> VNil
    VFalse -> VNil
    VTrue -> VCons VNil VNil
    VAtom atom -> VAtom (trans atom)
    VCons val1 val2 -> VCons (trans val1) (trans val2)
    VInt n -> foldr VCons VNil $ replicate (fromInteger n) VNil
