module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

-- These lists are used in the final function "showExp" along with my lookUp 
-- function to replace the expressions with their corresponding string 
-- representation.
binOps2 = [(Add, "+"), (Mul, "*"), (Div, "/")]
unOps2 = [(Neg, "-"), (Sin, "sin"), (Cos, "cos"), (Log, "log")]

-- Uses the inbuilt lookup funtion in Haskell to find objects in a given
-- lookup table, when given their corresponding identifier.  
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp id list = fromJust (lookup id list)

-- eval uses lookUp to replace each string in a given expression with it's 
-- operator from a set list of environments. It then evaluates the new 
-- expression.
eval :: Exp -> Env -> Double
eval (Val a) _  = a
eval (Id a) env = lookUp a env
eval (UnApp unOp b) env = (lookUp unOp unOps) (eval b env)
  where  unOps = [(Neg, (0-)), (Sin, sin)
                , (Cos, cos), (Log, log)] 
eval (BinApp binOp a b) env = (lookUp binOp binOps) (eval a env) (eval b env)
  where binOps = [(Add, (+)), (Mul, (*)), (Div, (/))]

-- diff takes a expression and differentiates it with respect to the 
-- corresponding string given then outputs the differentiated expression. 
-- The cases here involve differentials for all of the UnOps and BinOps 
-- including methods of differentiation such as the chain and quotient rule 
-- for multiplication and division respectively.         
diff :: Exp -> String -> Exp
diff (Val a) _ = Val 0  
diff (Id a) x 
  |a == x = (Val 1)
  |otherwise = (Val 0)
diff (UnApp unOp b) x 
  |unOp == Sin = BinApp Mul (UnApp Cos b) bDiff
  |unOp == Cos = UnApp Neg (BinApp Mul (UnApp Sin b) bDiff)
  |unOp == Neg = UnApp Neg bDiff
  |unOp == Log = BinApp Div bDiff b
  where 
    bDiff = (diff b x)  
diff (BinApp binOp a b) x 
  |binOp == Mul = BinApp Add (BinApp Mul a bDiff) (BinApp Mul aDiff b)
  |binOp == Add = BinApp Add aDiff bDiff
  |binOp == Div = BinApp Add left right
  where 
    left = BinApp Div (BinApp Mul b aDiff) (BinApp Mul b b) 
    right = UnApp Neg (BinApp Div (BinApp Mul b aDiff) (BinApp Mul b b))
    aDiff = (diff a x)
    bDiff = (diff b x)  
-- maclaurin takes an expression and outputs the value of its' 
-- corresponding maclaurin series as a double. It uses the inputted
-- double as the value of the variable when evaluation the iterated 
-- differential. The inputted in tells the function what we want the 
-- n'th term of the series to be (where it ends).
maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp inputVal nterm = sum zipl
  where zipl = (zipWith3 (\x y z -> (y * z) / x :: Double) facts coefs powers)
        diffhelp exp = diff exp "x"
        facts = map fromIntegral (scanl (*) 1 [(1)..(nterm)])
        coefs = map (`eval` [("x", 0)]) (take nterm (iterate diffhelp exp))
        powers  = (take nterm (iterate (inputVal*) 1))

-- Shows the string representation of an expression as in: 
-- (BinApp Add 5 3) ==> "5 + 3"
-- It uses appends a lot so I am aware this version of the function
-- is quite inefficient.
showExp :: Exp -> String
showExp (Val a) 
  = show a 
showExp (Id a) 
  = a  
showExp (UnApp unOp b) 
  = (lookUp unOp unOps2) ++ "(" ++ (showExp b) ++ ")"
showExp (BinApp binOpp a b) 
  = "(" ++ (showExp a) ++ (lookUp binOpp binOps2) ++ (showExp b) ++ ")"

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where


-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
