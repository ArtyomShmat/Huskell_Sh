-- Define a data type for algebraic expressions
data Expr
  = Const Double
  | Var String
  | UnaryOp String Expr
  | BinaryOp String Expr Expr
  deriving (Eq, Show)

-- Convert expression tree to string
exprToString :: Expr -> String
exprToString (Const c) = show c
exprToString (Var v) = v
exprToString (UnaryOp op e) = op ++ "(" ++ exprToString e ++ ")"
exprToString (BinaryOp op e1 e2) = "(" ++ exprToString e1 ++ " " ++ op ++ " " ++ exprToString e2 ++ ")"

-- Evaluate expression with given variable values
eval :: [(String, Double)] -> Expr -> Double
eval _ (Const c) = c
eval vars (Var v) = case lookup v vars of
  Just value -> value
  Nothing -> error $ "Variable " ++ v ++ " not found"
eval vars (UnaryOp "cos" e) = cos (eval vars e)
eval vars (UnaryOp "sqrt" e) = sqrt (eval vars e)
eval vars (BinaryOp "+" e1 e2) = eval vars e1 + eval vars e2
eval vars (BinaryOp "*" e1 e2) = eval vars e1 * eval vars e2
-- Add more operations as needed

-- Simplify expression
simplify :: Expr -> Expr
simplify (BinaryOp "+" (Const 0) e) = simplify e
simplify (BinaryOp "+" e (Const 0)) = simplify e
simplify (BinaryOp "*" (Const 1) e) = simplify e
simplify (BinaryOp "*" e (Const 1)) = simplify e
simplify (BinaryOp "*" (Const 0) _) = Const 0
simplify (BinaryOp "*" _ (Const 0)) = Const 0
simplify (BinaryOp "-" e1 e2) | e1 == e2 = Const 0
simplify (BinaryOp op e1 e2) = BinaryOp op (simplify e1) (simplify e2)
simplify (UnaryOp op e) = UnaryOp op (simplify e)
simplify e = e

-- Differentiate expression with respect to a variable
differentiate :: String -> Expr -> Expr
differentiate _ (Const _) = Const 0
differentiate var (Var v)
  | v == var = Const 1
  | otherwise = Const 0
differentiate var (UnaryOp "cos" e) = BinaryOp "*" (UnaryOp "-" (UnaryOp "sin" e)) (differentiate var e)
differentiate var (BinaryOp "+" e1 e2) = BinaryOp "+" (differentiate var e1) (differentiate var e2)
differentiate var (BinaryOp "*" e1 e2) = BinaryOp "+" (BinaryOp "*" (differentiate var e1) e2) (BinaryOp "*" e1 (differentiate var e2))
-- Add more differentiation rules as needed

-- Example usage
main :: IO ()
main = do
  let expr = BinaryOp "+" (Var "x") (Const 3)
  print $ exprToString expr
  print $ eval [("x", 5)] expr
  print $ exprToString $ simplify (BinaryOp "*" (Const 1) expr)
  print $ exprToString $ differentiate "x" expr