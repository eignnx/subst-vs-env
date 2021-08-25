{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Maybe (fromJust)

main :: IO ()
main = do
  let tm = App (Lam "x" TyInt (Var "x")) (Int 456)
  let env = []
  print $ infer env tm

data Tm
  = Int Int
  | Var String
  | Lam String Ty Tm
  | App Tm Tm
  | TyTmLam String Tm
  | TyTmApp Tm Ty
  deriving (Show)

data Ty
  = TyInt
  | TyVar String
  | FnTy Ty Ty
  | ForAll String Ty -- the type of `Lam X. tm`
  deriving (Show)

data TyVal
  = TVInt
  | TVFnTy TyVal TyVal
  | TVForAll Tcx String Ty
  deriving (Show)

type Tcx = [Binder]

data Binder
  = BindTmVar String TyVal
  | BindTyVar String (Maybe TyVal)
  deriving (Show)

lookupTmVar :: String -> Tcx -> TyVal
lookupTmVar x = \case
  (BindTmVar y ty : tcx)
    | x == y -> ty
    | otherwise -> lookupTmVar x tcx
  (_:tcx) -> lookupTmVar x tcx
  [] -> error $ "unbound tm var " ++ x

resolve :: Tcx -> Ty -> TyVal
resolve tcx = \case
  TyInt -> TVInt
  TyVar x -> lookupTmVar x tcx
  FnTy t1 t2 -> TVFnTy (resolve tcx t1) (resolve tcx t2)
  ForAll x ty -> undefined

infer :: Tcx -> Tm -> TyVal
infer tcx = \case
  Int _ -> TVInt
  Var x -> lookupTmVar x tcx
  Lam x xTy body -> TVFnTy xTy' bodyTy
    where bodyTy = infer tcx' body
          tcx'   = (BindTmVar x, xTy) : tcx
          xTy'   = resolve tcx xTy
  App fn arg -> let
    argTy = infer tcx arg
    TVFnTy paramTy retTy = infer tcx fn
    in if argTy == paramTy
          then retTy
          else error "wrong arg type"
  TyTmLam x body -> TVForAll tcx x bodyTy
    where bodyTy = infer tcx' body
          tcx'   = BindTyVar x Nothing : tcx
  TyTmApp fn ty -> let
    TVForAll creationTcx x bodyTy = infer tcx fn
    tcx' = BindTyVar x (Just ty) : creationTcx ++ tcx
    in infer tcx' bodyTy
  
------------------------------------------------

{-
data Val
  = VInt Int
  | VLam Env String Tm
  deriving (Show)

type Env = [(String, Val)]

eval :: Env -> Tm -> Val
eval env = \case
  Int x -> VInt x
  Var x -> fromJust $ lookup x env
  Lam x body -> VLam creationEnv x body
    where creationEnv = env
  App fn arg -> eval env' body
    where
      VLam creationEnv x body = eval env fn
      arg' = eval env arg
      env' = (x, arg') : creationEnv ++ env
-}

