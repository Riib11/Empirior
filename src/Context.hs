{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Context where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map            as Map hiding (map)

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
                        , _assignments  :: Map Name Expression
                        , _messages     :: [Message] }

data Message = Message
                { category :: MessageCategory
                , location :: String
                , title    :: String
                , body     :: String }

data MessageCategory = MessageComment
                     | MessageWarning
                     | MessageError
                     deriving (Eq)

initProgramContext :: ProgramContext
initProgramContext = ProgramContext emap emap emap emap []
  where emap = fromList []

makeLenses ''Message
makeLenses ''ProgramContext

instance Show ProgramContext where
  show ctx =
    let header1 = replicate 60 '='
        header2 = replicate 60 '-'
        showMap show field = (elems . Map.mapWithKey show $ ctx^.field)
        showList show field = map show (ctx^.field)
        showFunction    n   = show
        showPredicate   n   = show
        showDeclaration n t = n++" : "++ show t
        showAssignment  n e = n++" := "++show e
        showMessage         = show
    in
    unlines $
      [ header1, "| Program Context", header1
      , "| functions"    ]++showMap  showFunction    functions   ++[ header2
      , "| predicates"   ]++showMap  showPredicate   predicates  ++[ header2
      , "| declarations" ]++showMap  showDeclaration declarations++[ header2
      , "| assignments"  ]++showMap  showAssignment  assignments ++[ header2
      , "| messages"     ]++showList showMessage     messages    ++[ header1 ]

instance Show Message where
  show msg = unlines
    [ unwords [ show (msg&category), "at", msg&location ]
    , unwords [ " ", msg&title, ":", msg&body ] ]

instance Show MessageCategory where
  show = \case
    MessageError   -> "Error"
    MessageWarning -> "Warning"
    MessageComment -> "Comment"

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

-- functions

defineFunction :: Function -> ProgramState ()
defineFunction fun@(Function n _ _ _ _ _) = functions . at n .= Just fun

definitionOfFunction :: Name -> ProgramState Function
definitionOfFunction n = uses functions (!n)

-- predicates

definePredicate :: Predicate -> ProgramState ()
definePredicate pre@(Predicate n _ _) = predicates . at n .= Just pre

definitionOfPredicate :: Name -> ProgramState Predicate
definitionOfPredicate n = uses predicates (!n)

-- declarations

declare :: Name -> Type -> ProgramState ()
declare n t = declarations . at n .= Just t

declarationOf :: Name -> ProgramState Type
declarationOf n = uses declarations (!n)

-- assignments

assign :: Name -> Expression -> ProgramState ()
assign n e = assignments . at n .= Just e

assignmentOf :: Name -> ProgramState Expression
assignmentOf n = uses assignments (!n)

-- messages

message :: Message -> ProgramState ()
message m = messages %= (m:)

comment loc title body = message $ Message MessageComment loc title body
warn    loc title body = message $ Message MessageWarning loc title body
err     loc title body = message $ Message MessageError   loc title body

isError :: Message -> Bool
isError msg = (msg&category) == MessageError

hasErred :: ProgramState Bool
hasErred = any isError <$> use messages

unlessErred :: ProgramState () -> ProgramState ()
unlessErred s = do err <- hasErred ; unless err s
