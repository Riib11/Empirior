{-# LANGUAGE LambdaCase #-}

module Evaluation where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map            hiding (map)
import           Data.Maybe
import           Debug
import           Prelude

import           Context
import           Grammar


{-
  # Evaluation
-}

evaluateProgram :: Program -> ProgramState ()
evaluateProgram (Program s) = error "TODO"
-- evaluateProgram (Program s) = evaluateStatement s

{-
  ## Evaluate Statements
-}

evaluateStatement :: Statement -> ProgramState ()
evaluateStatement = \case
  StatementFunction fun@(Function n _ _ _ _ _) -> functions . at n .= Just fun

  StatementPredicate pre@(Predicate n _ _) -> predicates . at n .= Just pre

  StatementAssert p -> return ()

  StatementIfThenElse e s s' -> evaluateSubStatement s >> evaluateSubStatement s'

  StatementWhileLoop e p s -> evaluateSubStatement s

  StatementFold n as -> return ()

  StatementUnfold n as -> return ()

  StatementDeclaration n t -> assignments . at n .= (Just . error $ "undefined: "++n)

  StatementAssignment n e -> assignments . ix n .= e

  StatementSkip -> return ()

  StatementReturn e -> assignments . ix "result" .= e

  StatementSequence ss -> void $ traverse evaluateStatement ss

evaluateSubStatement :: Statement -> ProgramState ()
evaluateSubStatement = evalSubState . evaluateStatement

{-
  ## Evaluate Expressions
-}

evaluateExpression :: Expression -> ProgramState Expression
evaluateExpression = \case
  ExpressionValue v -> return $ ExpressionValue v

  ExpressionVariable x -> uses assignments (!x)

  ExpressionOperation o es ->
    let
      dft = case o of
          ExpressionAdd -> expressionInteger 0
          ExpressionSub -> expressionInteger 0
          ExpressionMul -> expressionInteger 1
          ExpressionEq  -> expressionTrue
          ExpressionNeq -> expressionTrue
          ExpressionGt  -> expressionTrue
          ExpressionGe  -> expressionTrue
          ExpressionLt  -> expressionTrue
          ExpressionLe  -> expressionTrue
          ExpressionAnd -> expressionTrue
          ExpressionOr  -> expressionTrue

      f e e' = do
        e  <- evaluateExpression e
        e' <- evaluateExpression e'
        return . ExpressionValue $ case (e, e') of
          (ExpressionValue v, ExpressionValue w) -> case (v, w) of
            (ValueInteger i, ValueInteger j) -> case o of
              ExpressionAdd -> ValueInteger $ i + j
              ExpressionSub -> ValueInteger $ i - j
              ExpressionMul -> ValueInteger $ i * j
              ExpressionEq  -> ValueBoolean $ i == j
              ExpressionNeq -> ValueBoolean $ i /= j
              ExpressionGt  -> ValueBoolean $ i >  j
              ExpressionGe  -> ValueBoolean $ i >= j
              ExpressionLt  -> ValueBoolean $ i <  j
              ExpressionLe  -> ValueBoolean $ i <= j
            (ValueBoolean a, ValueBoolean b) -> case o of
              ExpressionEq  -> ValueBoolean $ a == b
              ExpressionNeq -> ValueBoolean $ a /= b
              ExpressionAnd -> ValueBoolean $ a && b
              ExpressionOr  -> ValueBoolean $ a || b
          _ -> error "TODO: handle symbolics"
    in
    foldM f dft es

  ExpressionApplication n es -> do
    (Function n as t p q s) <- uses functions (!n)
    evalSubState $ do
      -- assign args
      void . traverse (uncurry $ \x -> (assignments . at x .=) . Just) $
        zip (map fst as) es
      -- assign old args
      -- TODO
      -- assign result to default
      assignments . at "result" .= (Just $ ExpressionValue ValueUnit)
      -- do dynamic checks from pre-condition
      -- TODO
      -- evaluate body
      evaluateStatement s
      -- do dynamic checks from post-condition
      -- TODO
      -- evaluate to result
      uses assignments (!"result")


{-
  ## Formula Simplification

  Simplify complex constructs in formulas.
  - Destruct each `unfolding a(es) in p` into `p`.
  - Destruct each `if e then p else q` into `(e && p) || (~ e && q)`.
  - Evaluate each contained expression
-}

simplifyFormula :: Formula -> ProgramState Formula
simplifyFormula (Formula g p) = case p of
  FormulaExpression e -> evaluateExpression e >>= \case
    ExpressionOperation eo es ->
      let fo = case eo of ExpressionAnd -> FormulaAnd ; ExpressionOr -> FormulaOr in
      return . Formula g $ FormulaOperation fo (map FormulaExpression es)
    e -> return . Formula g $ FormulaExpression e

  FormulaOperation o ps ->
    formulaOperation o <$> traverse (simplifyFormula . Formula Precise) ps

  FormulaPredication n es -> return . Formula g $ FormulaPredication n es

  FormulaIfThenElse e p q ->
    return . Formula g $ FormulaOperation FormulaOr
      [ FormulaOperation FormulaAnd [p, FormulaExpression e]
      , FormulaOperation FormulaAnd [p, FormulaNegation $ FormulaExpression e] ]

  FormulaUnfoldingIn n es p -> return . Formula g $ p

{-
  # Formula Normalization

  Convert a precise formula into set of disjunctions.
-}

normalizeFormula :: PreciseFormula -> ProgramState PreciseFormula
normalizeFormula = \case
  FormulaExpression e -> error "TODO"

  FormulaOperation o ps -> error "TODO"

  FormulaPredication n es -> error "TODO"

  p -> error $ "[!] Formula not simplified before normalization: "++show p
