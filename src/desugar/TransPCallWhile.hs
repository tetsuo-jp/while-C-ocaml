module TransPCallWhile where

import AbsWhile
import Control.Monad.State
import Data.List(insert)
import Data.Maybe(fromMaybe)
import Lib(true,false)


------------------------------------------------------------------------------
-- Inline procedure calls
------------------------------------------------------------------------------

-- mapping from procedure names to their bodies
type Penv = [(String,(Ident,[Com],Ident))]
type Rename a = State ([(String,String)],Penv,Int) a

mkPenv :: Program -> Penv
mkPenv (Prog procs) = f procs
 where f (AProc pNameOp ident1 coms ident2 : procs) = case pNameOp of
          NoName -> f procs
          Name (Ident name) -> (name,(ident1,coms,ident2)) : f procs
       f [] = error "No procedure has a procedure name."

doInline :: Program -> Program
doInline prog = evalState (inlineProgram prog) ([],mkPenv prog,0)

inlineProgram :: Program -> Rename Program
inlineProgram (Prog procs) = liftM Prog (mapM inlineProc procs)

inlineIdent :: Ident -> Rename Ident
inlineIdent (Ident str) = do
  (env,penv,n) <- get
  return $ Ident $ fromMaybe str (lookup str env)

inlineAtom x = return x

inlineProc (AProc pnameop ident1 coms ident2) =
    do coms' <- mapM inlineCom coms
       return $ AProc pnameop ident1 coms' ident2

inlineCom :: Com -> Rename Com
inlineCom x = case x of
      CAsn ident exp -> return $ CAsn ident exp
      CProc (Ident s2) (Ident pname) (Ident s1) ->
        do (env,penv,n) <- get
           let Just (Ident s1',coms,Ident s2') = lookup pname penv
           let vs = execState (vars coms) []
           let env' = (s1',s1) : (s2',s2) : zip vs (map (++"'"++show n) vs)
           put (env',penv,n+1)
           result <- mapM inlineCom coms
           (_,_,n') <- get
           put (env,penv,n')
           return $ CBlk result
      CLoop exp coms ->
        do coms' <- mapM inlineCom coms
           return $ CLoop exp coms'
      CIf exp coms cElseOp ->
        do coms' <- mapM inlineCom coms
           cElseOp' <- inlineCElseOp cElseOp
           return $ CIf exp coms' cElseOp'
      CCase exp patComTs ->
        do patComTs' <- mapM inlinePatComT patComTs
           return $ CCase exp patComTs'
      CBlk coms ->
        do coms' <- mapM inlineCom coms
           return $ CBlk coms'
      CShow exp -> return (CShow exp)

inlinePatComT (PatCom pat com) = do
  com' <- inlineCom com
  return $ PatCom pat com'

inlineCElseOp :: CElseOp -> Rename CElseOp
inlineCElseOp x = case x of
  ElseNone -> return ElseNone
  ElseOne coms -> do coms' <- mapM inlineCom coms
                     return $ ElseOne coms'


------------------------------------------------------------------------------
-- 変数をリストに集める
------------------------------------------------------------------------------

type VarsT a = State [String] a

pushS :: String -> VarsT ()
pushS str = do seen <- get
               put (insert str seen)

class Vars a where
  vars :: a -> VarsT a

instance Vars a => Vars [a] where
  vars xs = mapM vars xs

instance Vars Ident where
  vars x@(Ident str) =
    do pushS str
       return x

instance Vars Atom where
  vars x = return x

instance Vars Program where
  vars (Prog procs) =
    do ps <- mapM vars procs
       return $ Prog ps

instance Vars Proc where
  vars (AProc pnameop ident1 coms ident2) =
    do coms' <- mapM vars coms
       return $ AProc pnameop ident1 coms' ident2

instance Vars PNameOp where
  vars x = case x of
    Name ident -> liftM Name (vars ident)
    NoName -> return NoName

instance Vars Com where
  vars x = case x of
    CAsn ident exp -> liftM2 CAsn (vars ident) (vars exp)
    CProc ident1 ident2 ident3 -> 
      liftM3 CProc (vars ident1) (return ident2) (vars ident3)
    CLoop exp coms -> liftM2 CLoop (vars exp) (mapM vars coms)
    CShow exp -> liftM CShow (vars exp)

instance Vars Exp where
  vars x = case x of
    ECons exp1 exp2 -> liftM2 ECons (vars exp1) (vars exp2)
    EConsp exp -> do exp' <- vars exp
                     return $ EEq exp' (ECons (EHd exp') (ETl exp'))
    EAtomp ident -> liftM2 EEq (vars (EVal VFalse)) (vars (EConsp ident))
    EHd exp -> liftM EHd (vars exp)
    ETl exp -> liftM ETl (vars exp)
    EEq exp1 exp2 -> liftM2 EEq (vars exp1) (vars exp2)
    EListRep exps tl -> case tl of 
      NoTail -> liftM (foldr ECons (EVal VNil)) (mapM vars exps)
      Tail atom -> do a <- vars atom
                      liftM (foldr ECons (EVal (VAtom a))) (mapM vars exps)
    EVar ident -> liftM EVar (vars ident)
    EVal val -> liftM EVal (vars val)
    EConsStar exps -> liftM (foldr1 ECons) $ mapM vars exps
    EList exps -> liftM (foldr ECons (EVal VNil)) $ mapM vars exps

instance Vars Val where
  vars x = case x of
    VNil -> return VNil
    VFalse -> return VNil
    VTrue -> return $ VCons VNil VNil
    VAtom atom -> liftM VAtom (vars atom)
    VCons val1 val2 -> liftM2 VCons (vars val1) (vars val2)
    VInt n -> return $ foldr VCons VNil $ replicate (fromInteger n) VNil


------------------------------------------------------------------------------
-- Expand if, PDF pp.34-35
------------------------------------------------------------------------------

type RenameIf a = State Int a

expandIfProgram (Prog procs) = Prog (map expandIfProc procs)

expandIfProc x = case x of
    AProc pnameop ident1 coms ident2 ->
      AProc pnameop ident1 (concatMap expandIfCom coms) ident2

expandIfCom x = case x of
    CAsn ident exp -> [CAsn ident exp]
    CProc ident1 ident2 ident3 -> [CProc ident1 ident2 ident3]
    CLoop exp coms -> [CLoop exp (concatMap expandIfCom coms)]
    CIf exp coms celseop -> 
      case celseop of
        ElseNone -> 
          [CAsn (Ident "_Z") exp,
           CLoop (EVar (Ident "_Z")) (CAsn (Ident "_Z") false : coms)]
        ElseOne coms' ->
          [CAsn (Ident "_Z") exp,
           CAsn (Ident "_W") true,
           CLoop (EVar (Ident "_Z")) (CAsn (Ident "_Z") false : coms ++ [CAsn (Ident "_W") false]),
           CLoop (EVar (Ident "_W")) (CAsn (Ident "_W") false : coms')]
    CShow exp -> [CShow exp]
