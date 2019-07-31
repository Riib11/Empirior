{-# LANGUAGE TemplateHaskell #-}

module Context where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map            as Map

import           Grammar

{-
  # Program Context

  Complete context for program, used for
  - Typing
  - Interpretation
  - Verification
  - Evaluation.
-}

data ProgramContext = ProgramContext
                        { _functions    :: Map Name Function
                        , _predicates   :: Map Name Predicate
                        , _declarations :: Map Name Type
                        , _assignments  :: Map Name Expression }

initProgramContext = ProgramContext em em em em where em = fromList []

makeLenses ''ProgramContext

instance Show ProgramContext where
  show ctx =
    let header1 = replicate 60 '='
        header2 = replicate 60 '-'
        showMap show field = (elems . Map.mapWithKey show $ ctx^.field)
        showFunction    n   = show
        showPredicate   n   = show
        showDeclaration n t = n++" : "++ show t
        showAssignment  n e = n++" := "++show e in
    unlines $
      [ header1, "| Program Context", header1
      , "> functions"    ]++showMap showFunction    functions   ++[ header2
      , "> predicates"   ]++showMap showPredicate   predicates  ++[ header2
      , "> declarations" ]++showMap showDeclaration declarations++[ header2
      , "> assignments"  ]++showMap showAssignment  assignments ++[ header2 ]


{-
  ## Program State
-}

type ProgramState a = State ProgramContext a

evalSubState :: ProgramState a -> ProgramState a
evalSubState ps = evalState ps <$> get

execSubState :: ProgramState a -> ProgramState ()
execSubState ps = void.return.execState ps =<< get

{-
  ## Utilities
-}

defineFunction :: Function -> ProgramState ()
defineFunction fun@(Function n _ _ _ _ _) = functions . at n .= Just fun

definePredicate :: Predicate -> ProgramState ()
definePredicate pre@(Predicate n _ _) = predicates . at n .= Just pre

declare :: Name -> Type -> ProgramState ()
declare n t = declarations . at n .= Just t

declarationOf :: Name -> ProgramState Type
declarationOf n = uses declarations (!n)

assign :: Name -> Expression -> ProgramState ()
assign n e = assignments . at n .= Just e

assignmentOf :: Name -> ProgramState Expression
assignmentOf n = uses assignments (!n)
