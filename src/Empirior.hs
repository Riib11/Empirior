module Empirior where

import           Control.Monad.State

import           Context
import           Evaluation
import           Grammar
import           Interpretation
import           Typing
import           Verification

{-
  # Empirior

  1. Interpret Program
  2. Type Program
  3. Verify Program
  4. Evaluate Program
-}

run :: Program -> ProgramContext -> ProgramContext
run prgm = execState $ do
  interpretProgram prgm
  typeProgram prgm
  verifyProgram prgm
  evaluateProgram prgm
