{-# LANGUAGE LambdaCase #-}

module Grammar where

import           Control.Lens
import           Data.List
import           Data.Natural

{-
  # Program
-}

newtype Program = Program Statement

instance Show Program where
  show (Program s) = unlines ["Begin Program", show s, "End Program"]

{-
  # Statement
-}

data Statement = Function    Name [(Name, Type)] Type Formula Formula Statement
               | Predicate   Name [(Name, Type)] Formula
               | Assert      Formula
               | IfThenElse  Expression Statement Statement
               | WhileLoop   Expression Formula Statement
               | Declaration Name Type
               | Assignment  Name Expression
               | Skip
               | Return      Expression
               | Sequence    [Statement]

instance Show Statement where
  show =
    let showArgs = intercalate ", ".map (\(n,t) -> n++":"++show t) in
    \case
      Function n as t p q s -> unwords
                                [ "function", n, "("++showArgs as++")", "->", show t
                                , "\n  requires", show p
                                , "\n  ensures", show q
                                , "\n{\n  "++(
                                    case s of
                                      Sequence ss -> intercalate ";\n  " $ map show ss
                                      _           -> show s
                                    )++"\n}" ]
      Predicate n as p      -> unwords
                                [ "predicate", n, "("++showArgs as++")", ":=", show p ]
      Assert p              -> unwords ["assert", show p]
      IfThenElse e s s'     -> unwords
                                ["if", "("++show e++")", "then", "{", show s, "}"
                                , "else", "{", show s', "}"]
      WhileLoop e p s       -> unwords
                                ["while", "("++show e++")", "invariant", "(", show p, ")"
                                , "{", show s, "}"]
      Declaration n t       -> unwords [n, ":", show t]
      Assignment n e        -> unwords [n, ":=", show e]
      Skip                  -> unwords ["skip"]
      Return e              -> unwords ["return", show e]
      Sequence ss           -> intercalate ";\n" $ map show ss

{-
  # Formula
-}

data Formula = FormulaPrecise   PreciseFormula
             | FormulaImprecise PreciseFormula

instance Show Formula where
  show = \case
    FormulaPrecise p   -> show p
    FormulaImprecise p -> unwords ["?", show And, show p]

{-
  ## Precise Formula
-}

data PreciseFormula = FormulaExpression  Expression
                    | FormulaOperation   FormulaOperator Formula Formula
                    | FormulaPredication Name [Expression]
                    | FormulaIfThenElse  Expression Formula Formula

instance Show PreciseFormula where
  show =
    let showArgs = intercalate ", ".map show in
    \case
      FormulaExpression e     -> show e
      FormulaOperation o p q  -> unwords ["("++show p, show o, show q++")"]
      FormulaPredication n es -> unwords [n, "("++showArgs es++")"]
      FormulaIfThenElse e p q -> unwords [ "if", show e
                                         , "then", show p
                                         , "else", show q ]

data FormulaOperator = And | Or

instance Show FormulaOperator where
  show = \case
    And -> "/\\"
    Or  -> "\\/"
{-
  # Expression
-}

data Expression = ExpressionValue       Value
                | ExpressionVariable    Name
                | ExpressionApplication Name [Expression]
                deriving (Eq, Ord)

instance Show Expression where
  show =
    let showArgs = intercalate ", ".map show in
    \case
      ExpressionValue v          -> show v
      ExpressionVariable x       -> x
      ExpressionApplication n es -> unwords [n, "("++showArgs es++")"]

{-
  ## Value
-}

data Value = ValueUnit
           | ValueBoolean Bool
           | ValueNatural Natural
           deriving (Eq, Ord)

instance Show Value where
  show = \case
    ValueUnit -> "()"
    ValueBoolean b -> if b then "true" else "false"
    ValueNatural n -> show n

{-
  # Type
-}

data Type = TypeVoid
          | TypeUnit
          | TypeBoolean
          | TypeNatural
          | TypeFunction  [Type] Type
          | TypePredicate [Type]
          deriving (Eq, Ord)

instance Show Type where
  show = \case
    TypeVoid          -> "Void"
    TypeUnit          -> "Unit"
    TypeBoolean       -> "Boolean"
    TypeNatural       -> "Natural"
    TypeFunction ts t -> unwords ["("++intercalate " -> " (map show ts)++")", show t]


{-
  # Name
-}

type Name = String
