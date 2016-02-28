import Data.Char

type Operator = Char

data Expr = Num Int | Var String | Op Operator | App Expr Expr Expr
            deriving (Eq,Ord,Show)

type Token = Expr

data Associativity = L | N | R
                     deriving (Eq,Ord,Show)

ops :: [Operator]
ops = "+-*/^()$"

type Precedence = Int

opTable :: [(Operator, (Precedence, Associativity))]
opTable = [('$',(0,N)), ('(',(1,N)), ('+',(6,L)), ('-',(6,L)), 
           ('*',(7,L)), ('/',(7,L)), ('^',(8,R)), (')',(1,N))]

type ExprStack = [Expr]

type OpStack = [Operator]

showExpr :: Expr -> String
showExpr (Num n) 
  = show n 
showExpr (Var s) 
  = s
showExpr (Op c)  
  = [c]
showExpr (App op e e') 
  = "(" ++ showExpr e ++ showExpr op ++ showExpr e' ++ ")"

--------------------------------------------

--
-- Assume throughout that all function arguments are valid, for example:
--   All input expressions are well-formed
--   All Operators (Chars) are members of 'ops' above
--   The stacks passed to sbuildOpApp and parse constitute valid `state'
--   with respect to the Shunting Yard algorithm
--


-------------------------------------------------------------------
-- LookUp function
-- Pre : Operator must be in the opTable 
precedence :: Operator -> Precedence
precedence op = head [b | (a, (b,c)) <- opTable, op == a]

associativity :: Operator -> Associativity
associativity op = head [c | (a, (b,c)) <- opTable, op == a]

higherPrecedence :: Operator -> Operator -> Bool
higherPrecedence op1 op2 = precedence op1 > precedence op2

eqPrecedence :: Operator -> Operator -> Bool
eqPrecedence op1 op2 = precedence op1 == precedence op2

isRightAssociative :: Operator -> Bool
isRightAssociative op = associativity op == R

supersedes :: Operator -> Operator -> Bool
supersedes op1 op2 
  = higherPrecedence op1 op2 || eqPrecedence op1 op2 && isRightAssociative op1
   
stringToInt :: String -> Int
stringToInt = (foldl1 sum).(map (\x -> (ord x) - (ord '0')))
  where 
    sum x y = y + (x*10)

buildExpr :: String -> Expr
buildExpr s 
  = parse (tokenise s) ([], ['$'])

tokenise :: String -> [Token]
tokenise [] = []
tokenise l@(x : xs)
  |isSpace x = tokenise xs
  |elem x ops = (Op x) : tokenise xs
  |(x <= '9') && (x >= '0') = (Num (stringToInt firstToken)) : tokenise xs'
  |otherwise = Var firstToken : tokenise xs'
    where (firstToken, xs') = break p l
          p c = isSpace c || elem c ops
	    
buildOpApp :: (ExprStack, OpStack) -> (ExprStack, OpStack)
buildOpApp ((n : n' : es), (o : os))
  = (App (Op o) n' n : es, os)

parse :: [Token] -> (ExprStack, OpStack) -> Expr
parse [] ([e], os) = e
parse [] stacks = parse [] (buildOpApp stacks)
parse ((Op o) : ts) (es, (o' : os))
  |supersedes o o' = parse ts (es, (o : o' : os))
  |otherwise = parse ts (buildOpApp (es, (o' : o : os)))
parse (t : ts) (es, os) = parse ts ((t : es), os)  
