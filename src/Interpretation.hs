{-# LANGUAGE LambdaCase #-}

module Interpretation where

import           Control.Lens
import           Control.Monad.State

import           Context
import           Grammar

{-
  # Interpretation

  Interpret a program as well-formed:
  - functions and predicates are defined before reference
  - variables are declared before reference
-}

interpretProgram :: Program -> ProgramState ()
interpretProgram (Program s) = do
  interpretStatement s
  unlessErred $ comment "top"
    "Interpretation Successful"
    "There were no errors during interpretation."

{-
  ## Interpreting Statements
-}

interpretStatement :: Statement -> ProgramState ()
interpretStatement = \case
  StatementFunction fun@(Function n as t p q s) -> do
    execSubState $ do
      traverse interpretType (map snd as)
      interpretType t
      interpretFormula p
      interpretFormula q
      interpretStatement s
    defineFunction fun

  StatementPredicate pre@(Predicate n as p) -> do
    execSubState $ do
      traverse interpretType (map snd as)
      interpretFormula p
    definePredicate pre

  StatementAssert p -> interpretFormula p

  StatementIfThenElse e s s' -> do
    interpretExpression e
    execSubState $ interpretStatement s
    execSubState $ interpretStatement s'

  StatementWhileLoop e p s -> do
    interpretExpression e
    interpretFormula p
    execSubState $ interpretStatement s

  StatementFold n es -> do
    interpretPredicateName n
    void $ traverse interpretExpression es

  StatementUnfold n es -> do
    interpretPredicateName n
    void $ traverse interpretExpression es

  StatementDeclaration n t -> do
    declare n t
    interpretType t

  StatementAssignment n e -> do
    interpretVariableName n
    interpretExpression e

  StatementSkip -> return ()

  StatementReturn e -> interpretExpression e

  StatementSequence ss -> void $ traverse interpretStatement ss

{-
  ## Interpreting Formulas
-}

interpretFormula :: Formula -> ProgramState ()
interpretFormula (Formula g p) = interpretPreciseFormula p

interpretPreciseFormula :: PreciseFormula -> ProgramState ()
interpretPreciseFormula = \case
  FormulaExpression e -> interpretExpression e
  FormulaNegation p -> interpretPreciseFormula p
  FormulaOperation o ps -> void $ traverse interpretPreciseFormula ps
  FormulaPredication n es -> do
    interpretPredicateName n
    void $ traverse interpretExpression es
  FormulaIfThenElse e p q -> do
    interpretExpression e
    interpretPreciseFormula p
    interpretPreciseFormula q
  FormulaUnfoldingIn n es p -> do
    interpretPredicateName n
    void $ traverse interpretExpression es
    interpretPreciseFormula p

{-
  ## Interpreting Expressions
-}

interpretExpression :: Expression -> ProgramState ()
interpretExpression = \case
  ExpressionValue v -> return ()
  ExpressionVariable x -> interpretVariableName x
  ExpressionOperation o es -> void $ traverse interpretExpression es
  ExpressionApplication n es -> do
    interpretFunctionName n
    void $ traverse interpretExpression es

{-
  ## Interpreting Types
-}

interpretType :: Type -> ProgramState ()
interpretType = \case
  TypeVoid -> return ()
  TypeUnit -> return ()
  TypeBoolean -> return ()
  TypeInteger -> return ()
  TypeFunction ts t -> return ()
  TypePredicate ts -> return ()

{-
  ## Interpreting Names
-}

interpretFunctionName :: Name -> ProgramState ()
interpretFunctionName n = void (definitionOfFunction n)

interpretPredicateName :: Name -> ProgramState ()
interpretPredicateName n = void (definitionOfPredicate n)

interpretVariableName :: Name -> ProgramState ()
interpretVariableName n = void (declarationOf n)
