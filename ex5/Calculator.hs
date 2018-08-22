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

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr ExprT where
    lit value = Lit value
    add lhs rhs = Add lhs rhs
    mul lhs rhs = Mul lhs rhs

instance Expr Integer where
    lit value = value
    add lhs rhs = lhs + rhs
    mul lhs rhs = lhs * rhs

instance Expr Bool where
    lit value = value > 0
    add lhs rhs = lhs || rhs
    mul lhs rhs = lhs && rhs

instance Expr MinMax where
    lit value = MinMax value
    add lhs rhs = maximum [lhs, rhs]
    mul lhs rhs = minimum [lhs, rhs]

instance Expr Mod7 where
    lit value = Mod7 (mod value 7)
    add (Mod7 lhs) (Mod7 rhs) = Mod7 (mod (lhs + rhs) 7)
    mul (Mod7 lhs) (Mod7 rhs) = Mod7 (mod (lhs * rhs) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMinMax  = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7
