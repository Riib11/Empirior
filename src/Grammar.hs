{-# LANGUAGE LambdaCase #-}

module Grammar where

import           Control.Lens hiding (Fold)
import           Data.List

{-
  # Imperiror Grammar
-}

{-
  ## Program
-}

newtype Program = Program Statement

{-
  ### Show Instances
-}

instance Show Program where
  show (Program s) = unlines ["Begin Program", show s, "End Program"]

{-
  ## Statement
-}

data Statement = StatementFunction    Function
               | StatementPredicate   Predicate
               | StatementAssert      Formula
               | StatementIfThenElse  Expression Statement Statement
               | StatementWhileLoop   Expression Formula Statement
               | StatementFold        Name [Expression]
               | StatementUnfold      Name [Expression]
               | StatementDeclaration Name Type
               | StatementAssignment  Name Expression
               | StatementSkip
               | StatementReturn      Expression
               | StatementSequence    [Statement]

data Function = Function Name [(Name, Type)] Type Formula Formula Statement

data Predicate = Predicate Name [(Name, Type)] Formula

{-
  ### Show Instances
-}

instance Show Statement where
  show = \case
    StatementFunction fun -> show fun
    StatementPredicate pre -> show pre
    StatementAssert p -> unwords
      ["assert", show p]
    StatementIfThenElse e s s' -> unwords
      ["if", "("++show e++")", "then", "{", show s, "}"
      , "else", "{", show s', "}"]
    StatementWhileLoop e p s -> unwords
      ["while", "("++show e++")", "invariant", "(", show p, ")"
      , "{", show s, "}"]
    StatementFold n as -> unwords
      ["fold", n, "("++showArgs as++")"]
    StatementUnfold n as -> unwords
      ["unfold", n, "("++showArgs as++")"]
    StatementDeclaration n t -> unwords
      [n, ":", show t]
    StatementAssignment n e -> unwords
      [n, ":=", show e]
    StatementSkip ->
      unwords ["skip"]
    StatementReturn e ->
      unwords ["return", show e]
    StatementSequence ss ->
      intercalate ";\n" $ map show ss

instance Show Function where
  show (Function n as t p q s) = unwords
    [ "function"
    , n++
        (case as of [] -> ""
                    _  -> " ("++showParams as++")")
    , "->", show t
    , "\n  requires", show p
    , "\n  ensures", show q
    , "\n{\n  "++
        (case s of StatementSequence ss -> intercalate ";\n  " $ map show ss
                   _                    -> show s)
      ++"\n}" ]

instance Show Predicate where
  show (Predicate n as p) = unwords
    [ "predicate", n, "("++showParams as++")", ":=", show p ]

showParams :: Show a => [(Name, a)] -> String
showParams = intercalate ", ".map (\(n,t) -> n++":"++show t)

{-
  ## Formula
-}

data Formula = Formula Precision PreciseFormula

data Precision = Precise | Imprecise deriving (Show)

{-
  ### Show Instances
-}

instance Show Formula where
  show (Formula g p) = case g of
    Precise   -> show p
    Imprecise -> unwords ["?", show FormulaAnd, show p]

{-
  ## Precise Formula
-}

data PreciseFormula = FormulaExpression  Expression
                    | FormulaNegation    PreciseFormula
                    | FormulaOperation   FormulaOperator [PreciseFormula]
                    | FormulaPredication Name [Expression]
                    | FormulaIfThenElse  Expression PreciseFormula PreciseFormula
                    | FormulaUnfoldingIn Name [Expression] PreciseFormula

data FormulaOperator = FormulaAnd | FormulaOr

{-
  ### Show Instances
-}

instance Show PreciseFormula where
  show = \case
    FormulaExpression e       -> show e
    FormulaNegation p         -> unwords ["~", show p]
    FormulaOperation o ps     -> unwords ["("++intercalate (show o) (map show ps)++")"]
    FormulaPredication n es   -> unwords [n, "("++showArgs es++")"]
    FormulaIfThenElse e p q   -> unwords ["if", show e, "then", show p, "else", show q]
    FormulaUnfoldingIn n es p -> unwords [ "unfolding", n, "("++showArgs es++")"
                                         , "in", show p]

showArgs :: Show a => [a] -> String
showArgs = intercalate ", ".map show

instance Show FormulaOperator where
  show = \case
    FormulaAnd -> "/\\"
    FormulaOr  -> "\\/"
{-
  ## Expression
-}

data Expression = ExpressionValue       Value
                | ExpressionVariable    Name
                | ExpressionOperation   ExpressionOperator [Expression]
                | ExpressionApplication Name [Expression]
                deriving (Eq, Ord)

data ExpressionOperator = ExpressionAdd | ExpressionSub | ExpressionMul
                        | ExpressionGt  | ExpressionGe  | ExpressionLt | ExpressionLe
                        | ExpressionEq  | ExpressionNeq
                        | ExpressionAnd | ExpressionOr
  deriving (Eq, Ord)

{-
  ### Show Instances
-}

instance Show Expression where
  show = \case
    ExpressionValue v          -> show v
    ExpressionVariable x       -> x
    ExpressionApplication n es -> unwords [n, "("++showArgs es++")"]

instance Show ExpressionOperator where
  show = \case
    ExpressionAdd -> "+"
    ExpressionSub -> "-"
    ExpressionMul -> "*"
    ExpressionEq  -> "="
    ExpressionNeq -> "!="
    ExpressionGt  -> ">"
    ExpressionGe  -> ">="
    ExpressionLt  -> "<"
    ExpressionLe  -> "<="
    ExpressionAnd -> "&&"
    ExpressionOr  -> "||"

{-
  ## Value
-}

data Value = ValueUnit
           | ValueBoolean Bool
           | ValueInteger Integer
           deriving (Eq, Ord)

{-
  ### Show Instances
-}

instance Show Value where
  show = \case
    ValueUnit -> "unit"
    ValueBoolean b -> if b then "true" else "false"
    ValueInteger i -> show i

{-
  ## Type
-}

data Type = TypeVoid
          | TypeUnit
          | TypeBoolean
          | TypeInteger
          | TypeFunction  [Type] Type
          | TypePredicate [Type]
          deriving (Eq, Ord)

{-
  ### Show Instances
-}

instance Show Type where
  show = \case
    TypeVoid          -> "Void"
    TypeUnit          -> "Unit"
    TypeBoolean       -> "Boolean"
    TypeInteger       -> "Integer"
    TypeFunction ts t -> unwords ["("++showArgs ts++")", "->", show t]

{-
  ## Name
-}

type Name = String

{-
  # Utility Functions
-}

{-
  ## Constructor Abbreviations
-}

formulaBool  = FormulaExpression . ExpressionValue . ValueBoolean
formulaTrue  = formulaBool True
formulaFalse = formulaBool False

expressionInteger = ExpressionValue . ValueInteger
expressionBoolean = ExpressionValue . ValueBoolean
expressionTrue = expressionBoolean True
expressionFalse = expressionBoolean False

formulaOperation :: FormulaOperator -> [Formula] -> Formula
formulaOperation o ps = Formula (meetPrecisions $ map precision ps) $
                          FormulaOperation o (map precise ps)

{-
  ## Precision
-}

precise :: Formula -> PreciseFormula
precise (Formula _ p) = p

precision :: Formula -> Precision
precision (Formula h _) = h

meetPrecision :: Precision -> Precision -> Precision
meetPrecision h g = case (h,g) of
  (Precise, Precise) -> Precise
  _                  -> Imprecise

meetPrecisions :: [Precision] -> Precision
meetPrecisions = foldl meetPrecision Precise

{-
  ## Substitution
-}

substituteFormula :: Expression -> Name -> Formula -> Formula
substituteFormula f y (Formula h p) = Formula h (substitutePreciseFormula f y p)

substitutePreciseFormula :: Expression -> Name -> PreciseFormula -> PreciseFormula
substitutePreciseFormula f y = \case
  FormulaExpression  e     -> FormulaExpression $ substituteExpression f y e
  FormulaOperation   o ps  -> FormulaOperation o $ map (substitutePreciseFormula f y) ps
  FormulaPredication n es  -> FormulaPredication n $ map (substituteExpression f y) es
  FormulaIfThenElse  e p q -> FormulaIfThenElse (substituteExpression f y e)
                                                (substitutePreciseFormula f y p)
                                                (substitutePreciseFormula f y q)

substituteExpression :: Expression -> Name -> Expression -> Expression
substituteExpression f y = \case
  ExpressionValue v          -> ExpressionValue v
  ExpressionVariable x       -> if x == y then f else ExpressionVariable x
  ExpressionApplication n es -> ExpressionApplication n $
                                map (substituteExpression f y) es
