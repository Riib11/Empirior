module Verification where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Map
import           Prelude
import           System.IO.Unsafe


import           Context
import           Debug
import           Grammar
import           Implication
import           WeakestPrecondition

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
  -- verify all functions in program
  void $ traverse verifyFunction =<< uses functions elems
  -- verify top-level program
  isVerified <- do wp <- weakestPrecondition s (Formula Precise formulaTrue)
                   Formula Precise formulaTrue ==> wp
  unlessErred $ comment "top"
    "Program Verification Successful"
    "There were no errors during verification."

{-
  ## Verify Function
-}

verifyFunction :: Function -> ProgramState ()
verifyFunction (Function n as t p q s) = do
  wp <- weakestPrecondition s p
  isVerified <- p ==> wp
  if isVerified
    then comment ("function "++n)
      (n++" Verification Successful")
      "There were no errors during verification."
    else err ("function "++n)
      (n++"Verification Unsuccessful")
      "There were some errors during verification."
