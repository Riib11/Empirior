import           Control.Monad.State
import           Data.Map

import           Grammar
import           Typing

prgm = Program $ Sequence
  [ Function "f" [("x",TypeBoolean)] TypeBoolean
      (FormulaPrecise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (FormulaPrecise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (Sequence
        [ Return (ExpressionValue $ ValueBoolean True)
        ])
  ]


main :: IO ()
main = do
  putStrLn ""
  print prgm
  -- print $ execState (typeProgram prgm) (fromList [])
  -- putStrLn ""
  -- putStrLn $ replicate 20 '='
  -- putStrLn "[*] type checked"
