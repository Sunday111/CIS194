import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit value) = value
eval (Mul lhs rhs) = (eval lhs) * (eval rhs)
eval (Add lhs rhs) = (eval lhs) + (eval rhs)

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit value = Lit value
    add lhs rhs = Add lhs rhs
    mul lhs rhs = Mul lhs rhs