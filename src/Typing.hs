{-# LANGUAGE LambdaCase #-}

module Typing where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map            hiding (map)
import           Prelude
import           System.IO.Unsafe

import           Debug
import           Grammar

{-
  # Type Context
-}

type TypeContext = Map Expression Type

{-
  # Type Checking
-}

type TypeState a = State TypeContext a

typeSubTypeState :: TypeState a -> TypeState ()
typeSubTypeState s = void.return.execState s =<< get

{-
  ## Type Judgements
-}

judge :: Expression -> Type -> TypeState Type
judge e t = modify (insert e t) >> return t

typeOf :: Expression -> TypeState Type
typeOf e = gets (!e)

{-
  ## Type Checks
-}

mismatch :: Type -> Type -> a
mismatch t t' = error $ "type mismatch: "++show (t,t')

matchTypes :: Type -> Type -> TypeState Type
matchTypes t t' = if t == t' then return t else mismatch t t'

matchTypeStates :: TypeState Type -> TypeState Type -> TypeState Type
matchTypeStates s s' = do
  t <- s ; t' <- s'
  matchTypes t t'

{-
  ## Typing
-}

{-
  ### Typing Programs
-}

typeProgram :: Program -> TypeState ()
typeProgram (Program stmt) = void $ typeStatement stmt

{-
  ### Typing Statements
-}

typeStatement :: Statement -> TypeState Type
typeStatement = \case
  Function n args t p q s -> do
    typeSubTypeState $ do
      typeArguments args
      typeFormula p
      typeFormula q
      matchTypeStates (typeStatement s) (return t)
    ExpressionVariable n `judge` TypeFunction (map snd args) t

  Predicate n args p -> do
    typeSubTypeState $ do
      typeArguments args
      typeFormula p
    ExpressionVariable n `judge` TypePredicate (map snd args)

  Assert p -> do
    typeFormula p
    return TypeUnit

  IfThenElse e s s' -> do
    void $ matchTypeStates (typeExpression e) (return TypeBoolean)
    t <- typeStatement s ; t' <- typeStatement s'
    matchTypes t t'

  WhileLoop e p s -> do
    void $ matchTypeStates (typeExpression e) (return TypeBoolean)
    void $ typeFormula p
    typeStatement s

  Declaration n t -> do
    ExpressionVariable n `judge` t
    return TypeUnit

  Assignment n e -> do
    t  <- typeName n
    t' <- typeExpression e
    return.debug.show $ (t,t')
    matchTypes t t'
    return TypeUnit
    -- matchTypeStates (typeName n) (typeExpression e)

  Skip -> return TypeUnit

  Return e -> typeExpression e

  Sequence []     -> return TypeUnit
  Sequence [s]    -> typeStatement s
  Sequence (s:ss) -> do
    matchTypeStates (typeStatement s) (return TypeUnit)
    typeStatement $ Sequence ss

typeArguments :: [(Name, Type)] -> TypeState ()
typeArguments = void.traverse (\(x,t) -> ExpressionVariable x `judge` t)

{-
  ### Typing Arguments
-}

typeExpression :: Expression -> TypeState Type
typeExpression = \case
  ExpressionValue    v -> typeValue v
  ExpressionVariable n -> typeName n
  ExpressionApplication n es ->
    typeName n >>= \case
      TypeFunction ts t -> do
        ts' <- traverse typeExpression es
        void.traverse (uncurry matchTypes) $ zip ts ts'
        return t
      _ -> error $ "non-function application: "++show (ExpressionApplication n es)

typeValue :: Value -> TypeState Type
typeValue = \case
  ValueUnit      -> return TypeUnit
  ValueBoolean _ -> return TypeBoolean
  ValueNatural _ -> return TypeNatural

{-
  ### Typing Formulas
-}

typeFormula :: Formula -> TypeState ()
typeFormula = \case
  FormulaPrecise   p -> typePreciseFormula   p
  FormulaImprecise p -> typeImpreciseFormula p

typePreciseFormula :: PreciseFormula -> TypeState ()
typePreciseFormula = \case
  FormulaExpression e -> void $ matchTypeStates (typeExpression e) (return TypeBoolean)
  FormulaOperation o p q -> typeFormula p >> typeFormula q
  FormulaPredication n es  ->
    typeName n >>= \case
      TypePredicate ts -> do
        ts' <- traverse typeExpression es
        void.traverse (uncurry matchTypes) $ zip ts ts'
      _ -> error $ "non-predicate predication: "++show (FormulaPredication n es)

typeImpreciseFormula :: PreciseFormula -> TypeState ()
typeImpreciseFormula = typePreciseFormula

{-
  ### Typing Names
-}

typeName :: Name -> TypeState Type
typeName = typeOf.ExpressionVariable
