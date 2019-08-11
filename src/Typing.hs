{-# LANGUAGE LambdaCase #-}

module Typing where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map            hiding (foldl, map)
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
      matchedTypes (Function n as t p q s) t t'
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
    void $ typeExpression e >>= matchedTypes e TypeBoolean
    t  <- typeStatement s
    t' <- typeStatement s'
    matchedTypes (s, s') t t'

  StatementWhileLoop e p s -> do
    void $ typeExpression e >>= matchedTypes e TypeBoolean
    void $ typeFormula p
    typeStatement s

  StatementDeclaration n t -> do
    n `declare` t
    return TypeUnit

  StatementAssignment n e -> do
    t  <- typeExpression (ExpressionVariable n)
    t' <- typeExpression e
    matchedTypes (t, t') t t'
    return TypeUnit

  StatementSkip -> return TypeUnit

  StatementReturn e -> typeExpression e

  StatementSequence ss -> foldl (\_ s -> typeStatement s) (return TypeUnit) ss

  -- traverse typeStatement ss >> return TypeUnit

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
        traverse typeExpression es
          >>= void.traverse (\(t, t') -> matchedTypes (t, t') t t') . zip ts
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
  FormulaExpression e -> void $ typeExpression e >>= matchedTypes e TypeBoolean
  FormulaNegation p -> typePreciseFormula p
  FormulaOperation o ps -> void $ traverse typePreciseFormula ps
  FormulaPredication n es ->
    typeExpression (ExpressionVariable n) >>= \case
      TypePredicate ts -> traverse typeExpression es
                            >>= void . traverse (\(t, t') -> matchedTypes (t, t') t t')
                            . zip ts
      _ -> error $ "non-predicate predication: "++show (FormulaPredication n es)

{-
  # Type Checks
-}

mismatch :: Show a => a -> Type -> Type -> b
mismatch a t t' = error $ "type mismatch of "++show (t, t')++" in: "++show a

matchedTypes :: Show a => a -> Type -> Type -> ProgramState Type
matchedTypes a t t' = if t == t' then return t else mismatch a t t'
