module InlineWhile where

import AbsWhile
import Control.Monad.State
import Data.List(insert)
import Data.Maybe(fromMaybe)

type VarsT a = State [String] a
type Rename a = State ([(String,String)],Penv,Int) a

-- mapping from procedure names to their bodies
type Penv = [(String,(Ident,[Com],Ident))]

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
    do coms' <- inlineComs coms
       return $ AProc pnameop ident1 coms' ident2

inlinePNameOp x = case x of
    Name ident -> liftM Name (inlineIdent ident)
    NoName -> return NoName

inlineComs :: [Com] -> Rename [Com]
inlineComs [] = return []
inlineComs (x:xs) = do
    ys <- case x of
      CAsn ident exp ->
        do ident' <- inlineIdent ident
           exp' <- inlineExp exp
           return [CAsn ident' exp']
      CProc (Ident s2) (Ident pname) (Ident s1) -> 
        do (env,penv,n) <- get
           let Just (Ident s1',coms,Ident s2') = lookup pname penv
           let vs = execState (vars coms) []
           let env' = (s1',s1) : (s2',s2) : zip vs (map (++"'"++show n) vs)
           put (env',penv,n+1)
           result <- inlineComs coms
           (_,_,n') <- get
           put (env,penv,n')
           return result
      CLoop exp coms -> 
        do exp' <- inlineExp exp
           coms' <- inlineComs coms
           return [CLoop exp' coms']
      CShow exp -> 
        do exp' <- inlineExp exp
           return [CShow exp']
    xs' <- inlineComs xs
    return (ys ++ xs')

inlineExp x = case x of
    ECons exp1 exp2 -> liftM2 ECons (inlineExp exp1) (inlineExp exp2)
    EConsp ident -> do ident' <- inlineIdent ident
                       let x = EVar ident'
                       return $ EEq x (ECons (EHd x) (ETl x))
    EAtomp ident -> liftM2 EEq (inlineExp (EVal VFalse)) (inlineExp (EConsp ident))
    EHd exp -> liftM EHd (inlineExp exp)
    ETl exp -> liftM ETl (inlineExp exp)
    EEq exp1 exp2 -> liftM2 EEq (inlineExp exp1) (inlineExp exp2)
    EListRep exps tl -> case tl of 
      NoTail -> liftM (foldr ECons (EVal VNil)) (mapM inlineExp exps)
      Tail atom -> do a <- inlineAtom atom
                      liftM (foldr ECons (EVal (VAtom a))) (mapM inlineExp exps)
    EVar ident -> liftM EVar (inlineIdent ident)
    EVal val -> liftM EVal (inlineVal val)
    EConsStar exps -> liftM (foldr1 ECons) $ mapM inlineExp exps
    EList exps -> liftM (foldr ECons (EVal VNil)) $ mapM inlineExp exps

inlineVal x = case x of
    VNil -> return VNil
    VFalse -> return VNil
    VTrue -> return $ VCons VNil VNil
    VAtom atom -> liftM VAtom (inlineAtom atom)
    VCons val1 val2 -> liftM2 VCons (inlineVal val1) (inlineVal val2)
    VInt n -> return $ foldr VCons VNil $ replicate (fromInteger n) VNil

pushS :: String -> VarsT ()
pushS str = do seen <- get
               put (insert str seen)

-- 変数をリストに集める
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
    EConsp ident -> do ident' <- vars ident
                       let x = EVar ident'
                       return $ EEq x (ECons (EHd x) (ETl x))
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
