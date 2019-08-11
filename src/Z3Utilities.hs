module Z3Utilities where

import           Control.Monad.Trans
import           Control.Monad.Trans.State
import           Z3.Base                   as Z3B

import           Context

{-
  ## Lifting Z3 Functions
-}

liftZ3Const :: (Z3B.Context -> a) -> ProgramState a
liftZ3Const f = do
  zc <- gets z3Context
  return $ f zc

liftZ3 :: (Z3B.Context -> a -> b) -> a -> ProgramState b
liftZ3 f a = do
  zc <- gets z3Context
  return $ f zc a

liftZ3IO :: (Z3B.Context -> a -> IO b) -> a -> ProgramState b
liftZ3IO f a = do
  zc <- gets z3Context
  lift . f zc $ a

liftZ3ConstIO :: (Z3B.Context -> IO a) -> ProgramState a
liftZ3ConstIO f = lift . f =<< gets z3Context
