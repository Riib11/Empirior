{-# LANGUAGE LambdaCase #-}

module Typing where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map            hiding (map)
import           Prelude
import           System.IO.Unsafe

import           Context
import           Debug
import           Grammar

{-
  # Typing

  Construct the type of the appropriate parts of program:
  - function applications use correct argument types
  - predications use correct argument types
  - predicate-expressions yield booleans
  - assignments have matching declared and expression type
  -

-}

typeProgram :: Program -> ProgramState ()
typeProgram (Program stmt) = do
  void $ typeStatement stmt
  unlessErred $ comment "top"
    "Typing Successful"
    "There were no errors during typing."

{-
  ## Typing Statements
-}

typeStatement :: Statement -> ProgramState Type
typeStatement = \case
  StatementFunction (Function n as t p q s) -> do
    evalSubState $ do
      traverse (uncurry declare) as
      typeFormula p
      typeFormula q
      t' <- typeStatement s
      matchedTypes t t'
    n `declare` TypeFunction (map snd as) t
    declarationOf n

  StatementPredicate (Predicate n as p) -> do
    evalSubState $ do
      traverse (uncurry declare) as
      typeFormula p
    n `declare` TypePredicate (map snd as)
    declarationOf n

  StatementAssert p -> do
    typeFormula p
    return TypeUnit

  StatementIfThenElse e s s' -> do
    void $ typeExpression e >>= matchedTypes TypeBoolean
    t  <- typeStatement s
    t' <- typeStatement s'
    matchedTypes t t'

  StatementWhileLoop e p s -> do
    void $ typeExpression e >>= matchedTypes TypeBoolean
    void $ typeFormula p
    typeStatement s

  StatementDeclaration n t -> do
    n `declare` t
    return TypeUnit

  StatementAssignment n e -> do
    t  <- typeExpression (ExpressionVariable n)
    t' <- typeExpression e
    matchedTypes t t'
    return TypeUnit

  StatementSkip -> return TypeUnit

  StatementReturn e -> typeExpression e

  StatementSequence ss -> traverse typeStatement ss >> return TypeUnit

{-
  ## Typing Expressions
-}

typeExpression :: Expression -> ProgramState Type
typeExpression = \case
  ExpressionValue v          -> typeValue v
  ExpressionVariable n       -> declarationOf n
  ExpressionApplication n es ->
    typeExpression (ExpressionVariable n) >>= \case
      TypeFunction ts t -> do
        traverse typeExpression es >>= void.traverse (uncurry matchedTypes) . zip ts
        return t
      _ -> error $ "non-function application: "++show (ExpressionApplication n es)

typeValue :: Value -> ProgramState Type
typeValue = \case
  ValueUnit      -> return TypeUnit
  ValueBoolean _ -> return TypeBoolean
  ValueInteger _ -> return TypeInteger

{-
  ## Typing Formulas
-}

typeFormula :: Formula -> ProgramState ()
typeFormula (Formula g p) = typePreciseFormula p

typePreciseFormula :: PreciseFormula -> ProgramState ()
typePreciseFormula = \case
  FormulaExpression e -> void $ typeExpression e >>= matchedTypes TypeBoolean
  FormulaNegation p -> typePreciseFormula p
  FormulaOperation o ps -> void $ traverse typePreciseFormula ps
  FormulaPredication n es ->
    typeExpression (ExpressionVariable n) >>= \case
      TypePredicate ts -> traverse typeExpression es
                            >>= void . traverse (uncurry matchedTypes) . zip ts
      _ -> error $ "non-predicate predication: "++show (FormulaPredication n es)

{-
  # Type Checks
-}

mismatch :: Type -> Type -> a
mismatch t t' = error $ "type mismatch: "++show (t,t')

matchedTypes :: Type -> Type -> ProgramState Type
matchedTypes t t' = if t == t' then return t else mismatch t t'
