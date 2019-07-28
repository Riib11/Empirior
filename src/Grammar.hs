module Grammar where

import           Data.Natural

{-
  # Program
-}

newtype Program = Program Statement deriving (Show)

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
               deriving (Show)

{-
  # Formula
-}

data Formula = FormulaPrecise   PreciseFormula
             | FormulaImprecise PreciseFormula
             deriving (Show)

{-
  ## Precise Formula
-}

data PreciseFormula = FormulaExpression  Expression
                    | FormulaOperation   FormulaOperator Formula Formula
                    | FormulaPredication Name [Expression]
                    | FormulaIfThenElse  Expression Formula Formula
                    deriving (Show)

data FormulaOperator = And | Or deriving (Show)

{-
  # Expression
-}

data Expression = ExpressionValue       Value
                | ExpressionVariable    Name
                | ExpressionApplication Name [Expression]
                deriving (Show, Eq, Ord)

{-
  ## Value
-}

data Value = ValueUnit
           | ValueBoolean Bool
           | ValueNatural Natural
           deriving (Show, Eq, Ord)

{-
  # Type
-}

data Type = TypeVoid
          | TypeUnit
          | TypeBoolean
          | TypeNatural
          | TypeFunction [Type] Type
          | TypePredicate [Type]
          deriving (Show, Eq, Ord)

{-
  # Name
-}

type Name = String
