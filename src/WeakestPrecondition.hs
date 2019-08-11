module WeakestPrecondition where

import           Control.Lens hiding (Fold)

import           Context
import           Grammar

weakestPrecondition :: Statement -> Formula -> ProgramState Formula
weakestPrecondition s wp = case s of
  StatementFunction (Function n as t p q s) -> return wp

  StatementPredicate (Predicate n as p) -> return wp

  StatementAssert p -> return $ formulaOperation FormulaAnd [p, wp]

  StatementIfThenElse e s s' -> do
    p <- weakestPrecondition s wp
    q <- weakestPrecondition s' wp
    return $ formulaOperation FormulaAnd
      [ Formula Precise $ FormulaIfThenElse e (precise p) (precise q), wp ]

  StatementWhileLoop e p s -> do
    q <- weakestPrecondition s (formulaOperation FormulaAnd [p, wp])
    return $ formulaOperation FormulaAnd
      [ Formula Precise $ FormulaExpression e
      , Formula (precision q) $ FormulaIfThenElse e (precise q) (formulaBool True)
      , wp ]

  StatementFold n as -> error "TODO"

  StatementUnfold n as -> error "TODO"

  StatementDeclaration n t -> return wp

  StatementAssignment n e -> return $ substituteFormula e n wp

  StatementSkip -> return wp

  StatementReturn e -> return $ substituteFormula e "result" wp

  StatementSequence ss -> foldr (\s -> (weakestPrecondition s =<<)) (return wp) ss
