import           Control.Monad.State
import           Data.Map

import           Context
import           Evaluation
import           Grammar
import           Typing

prgm = Program $ StatementSequence
  [ StatementFunction $ Function "f" [("x",TypeBoolean)] TypeBoolean
      (Formula Precise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (Formula Precise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (StatementSequence
        [ StatementReturn (ExpressionValue $ ValueInteger 1)
        ])
  ]

form =
  let exprBool = ExpressionValue . ValueBoolean
      exprInt  = ExpressionValue . ValueInteger
  in
  Formula Precise . FormulaExpression $
    ExpressionApplication "f" []
    -- ExpressionOperation ExpressionAdd [exprInt 1, exprInt 2]
    -- ExpressionOperation ExpressionOr [exprBool True, exprBool True, exprBool False]

main :: IO ()
main = do
  putStrLn ""
  -- testFormulaSimplification
  testTyping

-- testFormulaSimplification =
--   let ctx = EvaluationContext
--               { _functions = fromList
--                   [ ("f", Function "f" [] TypeInteger
--                       (Formula Precise formulaTrue)
--                       (Formula Precise formulaTrue)
--                       (StatementReturn $ ExpressionValue (ValueInteger 1))) ]
--               , _predicates = fromList []
--               , _variables = fromList [] }
--       form' = evalState (simplifyFormula form) ctx
--   in do
--     putStrLn $ replicate 20 '='
--     putStrLn $ show form++" ==> "++show form'

testTyping = do
  print $ execState (typeProgram prgm) initProgramContext
  putStrLn $ replicate 20 '='
  putStrLn "[*] type checked"
