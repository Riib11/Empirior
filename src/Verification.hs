module Verification where

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
  # Verification

  Verify that the program and each of its functions meet their specifications.
  - The program has a pre- and post-condition of `true`
  - Each function is verified modularly
  - Imprecise formulas allow additional conjuncts to be statically derived,
      which are enforced during evaluation.
-}

verifyProgram :: Program -> ProgramState ()
verifyProgram (Program s) = do
  verifyStatement s
  unlessErred $ comment "top"
    "Verification Successful"
    "There were no errors during verification."

{-
  ## Verify Statement
-}

verifyStatement :: Statement -> ProgramState ()
verifyStatement s = error "TODO"

{-
  ## Verify Function
-}

verifyFunction :: Function -> ProgramState ()
verifyFunction = error "TODO"
