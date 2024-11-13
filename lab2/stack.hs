import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

-- Define a data type for instructions
data Instruction
  = Push Int
  | PushVar String
  | Add
  | Mul
  | Sub
  | Div
  | Print
  deriving (Show, Eq)

-- Define a type for the stack and environment
type Stack = [Int]
type Env = Map String Int

-- Define a function to run the program
runProgram :: [Instruction] -> Env -> Either String Stack
runProgram instructions env = foldl execute (Right []) instructions
  where
    execute :: Either String Stack -> Instruction -> Either String Stack
    execute (Left err) _ = Left err
    execute (Right stack) instr = case instr of
      Push n -> Right (n : stack)
      PushVar var -> case Map.lookup var env of
        Just val -> Right (val : stack)
        Nothing -> Left $ "Variable " ++ var ++ " not found"
      Add -> binaryOp (+) stack
      Mul -> binaryOp (*) stack
      Sub -> binaryOp (-) stack
      Div -> case stack of
        (x:y:ys) -> if x == 0 then Left "Division by zero" else Right ((y `div` x) : ys)
        _ -> Left "Insufficient operands for division"
      Print -> case stack of
        (_:xs) -> Right xs
        _ -> Left "Insufficient operands for print"

    binaryOp :: (Int -> Int -> Int) -> Stack -> Either String Stack
    binaryOp op (x:y:ys) = Right ((y `op` x) : ys)
    binaryOp _ _ = Left "Insufficient operands for operation"

-- Function to parse a program from text
parseProgram :: String -> Either String [Instruction]
parseProgram = mapM parseLine . lines
  where
    parseLine line = case words line of
      ["push", n] -> case readMaybe n of
        Just num -> Right (Push num)
        Nothing -> Right (PushVar n)  -- Assume it's a variable
      ["add"] -> Right Add
      ["mul"] -> Right Mul
      ["sub"] -> Right Sub
      ["div"] -> Right Div
      ["print"] -> Right Print
      _ -> Left $ "Invalid instruction: " ++ line

-- Example expression tree
data Expr
  = Const Int
  | Var String
  | AddExpr Expr Expr
  | MulExpr Expr Expr
  deriving (Show, Eq)

-- Convert expression tree to stack machine program
exprToProgram :: Expr -> [Instruction]
exprToProgram (Const n) = [Push n]
exprToProgram (Var v) = [PushVar v]
exprToProgram (AddExpr e1 e2) = exprToProgram e1 ++ exprToProgram e2 ++ [Add]
exprToProgram (MulExpr e1 e2) = exprToProgram e1 ++ exprToProgram e2 ++ [Mul]

-- Example program
exampleProgram :: String
exampleProgram = unlines
  [ "push 5"
  , "push 4"
  , "push x"
  , "mul"
  , "add"
  ]

-- Run the example
main :: IO ()
main = do
  let env = Map.fromList [("x", 3)]
  case parseProgram exampleProgram of
    Left err -> putStrLn $ "Parse Error: " ++ err
    Right program -> case runProgram program env of
      Left err -> putStrLn $ "Runtime Error: " ++ err
      Right stack -> putStrLn $ "Final stack: " ++ show stack