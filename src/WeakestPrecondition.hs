module WeakestPrecondition where

import           Control.Lens hiding (Fold)

import           Grammar

-- TODO: move this to Verification module, so that can access VerificationContext (needs access to evaluations for implication, functions, predicates, etc)
weakestPrecondition :: Statement -> Formula -> Formula
weakestPrecondition s wp = case s of
  StatementFunction (Function n as t p q s) -> wp

  StatementPredicate (Predicate n as p) -> wp

  StatementAssert p -> formulaOperation FormulaAnd [p, wp]

  StatementIfThenElse e s s' ->
    let p = weakestPrecondition s wp
        q = weakestPrecondition s' wp in
    formulaOperation FormulaAnd
      [ Formula Precise $ FormulaIfThenElse e (precise p) (precise q), wp ]

  StatementWhileLoop e p s ->
    let q = weakestPrecondition s (formulaOperation FormulaAnd [p, wp]) in
    formulaOperation FormulaAnd
      [ Formula Precise $ FormulaExpression e
      , Formula (precision q) $ FormulaIfThenElse e (precise q) (formulaBool True)
      , wp ]

  StatementFold n as -> error "TODO"

  StatementUnfold n as -> error "TODO"

  StatementDeclaration n t -> wp

  StatementAssignment n e -> substituteFormula e n wp

  StatementSkip -> wp

  StatementReturn e -> substituteFormula e "result" wp

  StatementSequence ss -> foldr weakestPrecondition wp ss
