module Interpret (
    Context,
    createContext,
    getValue,
    evalProg,
    evalProgCtx
  ) where

import Synt
import Data.Map as M
import Data.Bits

newtype Context = Context { impl :: Map String Int }
instance Show Context where
  show ctx =
    concatMap (\(k,v) -> "  " ++ k ++ " = " ++ show v ++ "\n")
      $ toList.impl $ ctx

createContext :: Context
createContext = Context { impl = empty }

getValue :: Context -> String -> Maybe Int
getValue ctx name = M.lookup name $ impl ctx

evalProg :: Program -> Either String Context
evalProg prog = evalProgCtx createContext prog

evalProgCtx :: Context -> Program -> Either String Context
evalProgCtx ctx (Program lst) =
  evalProg' ctx lst
  where
    evalProg' ctx (ExprList x xs) = do
      rslt <- evalExpr ctx x
      evalProg' rslt xs
    evalProg' ctx ExprEnd = return ctx

evalExpr :: Context -> Expr -> Either String Context
evalExpr ctx (Expr vname rval) = do
  val <- evalRVal ctx rval
  return $ Context { impl = insert vname val $ impl ctx }

evalError :: String -> Either String Int
evalError s = Left s

evalRVal :: Context -> RVal -> Either String Int

evalRVal ctx (IdentVal ident) =
  case getValue ctx ident of
    Just rval -> Right rval
    Nothing -> Left $ "undefined variable '" ++ ident ++ "'"

evalRVal ctx (BinOp cmd a b) = do
  let f = evalRVal ctx
  x <- f a
  y <- f b
  evalBinOp cmd x y

evalRVal ctx (UnOp cmd a) = do
  x <- evalRVal ctx a
  evalUnOp cmd x

evalRVal _ (IntVal x) = return x

evalUnOp cmd x = do
  return $ case cmd of
    Neg -> (-x)
    Pos -> if x < 0 then (-x) else (x)

evalBinOp cmd x y = do
    return $ case cmd of
      Add -> x + y
      Sub -> x - y
      Mul -> x * y
