import           Control.Lens
import           Control.Monad.State
import           Data.Map

import           Context
import           Evaluation
import           Grammar
import           Interpretation
import           Typing
import           Verification

prgm = Program $ StatementSequence
  [ StatementFunction $ Function "f" [("x",TypeBoolean)] TypeBoolean
      (Formula Precise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (Formula Precise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (StatementSequence
        [ StatementReturn (ExpressionValue $ ValueBoolean True)
        ])
  , StatementFunction $ Function "g" [] TypeBoolean
      (Formula Precise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (Formula Precise $ FormulaExpression $ ExpressionValue $ ValueBoolean True)
      (StatementSequence
        [ StatementReturn (ExpressionValue $ ValueBoolean False)
        ])
  , StatementAssert form
  ]

form =
  let exprBool = ExpressionValue . ValueBoolean
      exprInt  = ExpressionValue . ValueInteger
  in
  Formula Precise . FormulaExpression $ ExpressionValue (ValueBoolean True)
    -- ExpressionApplication "f" [exprInt 1]
    -- ExpressionOperation ExpressionAdd [exprInt 1, exprInt 2]
    -- ExpressionOperation ExpressionOr [exprBool True, exprBool True, exprBool False]

test = do
  interpretProgram prgm
  typeProgram prgm
  verifyProgram prgm

main :: IO ()
main = do
  putStrLn ""
  s <- initProgramContext
  s <- execStateT test s
  print s

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
