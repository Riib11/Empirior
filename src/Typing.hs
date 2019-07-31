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
  ## Type Checks
-}

mismatch :: Type -> Type -> a
mismatch t t' = error $ "type mismatch: "++show (t,t')

matchedTypes :: Type -> Type -> ProgramState Type
matchedTypes t t' = if t == t' then return t else mismatch t t'

{-
  ## Typing
-}

{-
  ### Typing Programs
-}

typeProgram :: Program -> ProgramState ()
typeProgram (Program stmt) = void $ typeStatement stmt

{-
  ### Typing Statements
-}

typeStatement :: Statement -> ProgramState Type
typeStatement = \case
  StatementFunction (Function n args t p q s) -> do
    evalSubState $ do
      typeArguments args
      typeFormula p
      typeFormula q
      t' <- typeStatement s
      return $! debug . show $ t'
      matchedTypes t t'
    n `declare` TypeFunction (map snd args) t
    declarationOf n

  StatementPredicate (Predicate n args p) -> do
    evalSubState $ do
      typeArguments args
      typeFormula p
    n `declare` TypePredicate (map snd args)
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
    t <- typeExpression e
    void $ matchedTypes t TypeBoolean
    void $ typeFormula p
    typeStatement s

  StatementDeclaration n t -> do
    n `declare` t
    return TypeUnit

  StatementAssignment n e -> do
    t  <- typeExpression (ExpressionVariable n)
    t' <- typeExpression e
    return.debug.show $ (t, t')
    matchedTypes t t'
    return TypeUnit

  StatementSkip -> return TypeUnit

  StatementReturn e -> typeExpression e

  StatementSequence ss -> foldM (\_ s -> typeStatement s) TypeUnit ss

typeArguments :: [(Name, Type)] -> ProgramState ()
typeArguments = void.traverse (uncurry declare)

{-
  ### Typing Arguments
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
  ### Typing Formulas
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
