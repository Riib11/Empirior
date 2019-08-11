module Implication where

import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Z3.Base                   as Z3B

import           Context
import           Formula
import           Grammar
import           Z3Utilities

implies :: Formula -> Formula -> ProgramState Bool
implies (Formula g p) (Formula h q) = case (g, h) of
  (Precise,   Precise)   -> impliesPrecise p q
  (Imprecise, Precise)   -> error "TODO: imprecise implication"
  (Precise,   Imprecise) -> error "TODO: imprecise implication"
  (Imprecise, Imprecise) -> error "TODO: imprecise implication"

(==>) = implies

impliesPrecise :: PreciseFormula -> PreciseFormula -> ProgramState Bool
impliesPrecise p q = do
  zc <- gets z3Context

  sp <- symbolicPreciseFormula p
  sq <- symbolicPreciseFormula q
  imp <- lift $ mkImplies zc sp sq

  sol <- lift $ mkSolver zc

  lift $ solverAssertCnstr zc sol imp

  res <- lift $ solverCheck zc sol
  case res of
    Sat -> return True
    _   -> return False
