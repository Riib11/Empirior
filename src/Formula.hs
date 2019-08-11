{-# LANGUAGE LambdaCase #-}

module Formula where

import           Control.Lens
import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Data.Map                  as Map hiding (map)
import           Z3.Base                   as Z3B

import           Context
import           Grammar
import           Z3Utilities

{-
  # Symbolic Precise Formula
-}

symbolicPreciseFormula :: PreciseFormula -> ProgramState AST
symbolicPreciseFormula = \case

  FormulaExpression e -> do
    se <- symbolicExpression e
    seSort <- liftZ3IO getSort se
    booleanSort <- uses z3Sorts (!"Boolean")
    if seSort /= booleanSort
      then error $ "[!] non-boolean formula-level expression: "++show e
      else return se

  FormulaNegation p -> liftZ3IO mkNot =<< symbolicPreciseFormula p

  FormulaOperation o ps -> do
    sps <- traverse symbolicPreciseFormula ps
    case o of
      FormulaAnd -> liftZ3IO mkAnd sps
      FormulaOr  -> liftZ3IO mkOr  sps

  FormulaPredication n es -> error "TODO"

  FormulaIfThenElse e p p' -> error "TODO"


{-
  ## Symbolic Expression
-}

symbolicExpression :: Expression -> ProgramState AST
symbolicExpression = \case
  ExpressionValue v -> case v of
    ValueUnit      -> liftZ3IO mkBool    True
    ValueBoolean b -> liftZ3IO mkBool    b
    ValueInteger i -> liftZ3IO mkInteger i

  ExpressionVariable x -> uses declarations (!x) >>= \case
    TypeUnit -> do
      v <- liftZ3IO mkBoolVar =<< liftZ3IO mkStringSymbol x
      x <- liftZ3IO mkBool True
      zc <- gets z3Context
      lift $ mkEq zc v x
    TypeBoolean -> liftZ3IO mkBoolVar =<< liftZ3IO mkStringSymbol x
    TypeInteger -> liftZ3IO mkIntVar =<< liftZ3IO mkStringSymbol x

  ExpressionApplication n es -> do
    (Function n as t p q s) <- uses functions (!n)
    nameSym <- liftZ3IO mkStringSymbol n
    argSorts <- traverse sortFromType (map snd as)
    resSort <- sortFromType t
    zc <- gets z3Context
    funcDecl <- lift $ mkFuncDecl zc nameSym argSorts resSort
    ses <- traverse symbolicExpression es
    zc <- gets z3Context
    lift $ mkApp zc funcDecl ses

{-
  ## Z3 Utility Functions
-}

sortFromType :: Type -> ProgramState Sort
sortFromType = \case
  TypeVoid    -> error "TODO"
  TypeUnit    -> uses z3Sorts (!"Unit")
  TypeBoolean -> uses z3Sorts (!"Boolean")
  TypeInteger -> uses z3Sorts (!"Integer")
  TypeFunction t ts -> error "[!] TypeFunction not allowed as symbolic type"
  TypePredicate ts -> error "[!] TypePredicate not allowed as symbolic type"
