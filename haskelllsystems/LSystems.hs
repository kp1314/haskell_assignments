module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: System -> Float
angle (a, c, r) = a

-- Returns the base string for the given system.
base :: System -> String
base  (a, c, r) = c

-- Returns the set of rules for the given system.
rules :: System -> Rules
rules (a, c, r) = r

-- Looks up a character in the given set of rules and 
-- replaces it with the corresponding command.

--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar character ((id, cstring) : cstrings)
  |character == id = cstring
  |otherwise = lookupChar character cstrings 

-- Expands a command once using the given set of rules 
-- and the lookup function to give a string consisting of
-- the interpreted commands
expandOne :: Rules -> String -> String
expandOne r [] = []
expandOne r (character : characters)
  = lookupChar character r ++ expandOne r characters

-- Expands a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand r cstring n
  |n == 0 = cstring
  |otherwise = expand r (expandOne r cstring) (n - 1)
	
-- Moves a turtle using trigonnometry to work out the set 
-- units a turtle moves with a given command.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move movement ((x,y), angle1) angle2
  |movement == 'F' = ((x + cos(changeAngle1), y + sin(changeAngle1)), angle1)
  |movement == 'L' = ((x, y), angle1 + angle2)
  |otherwise = ((x, y), angle1 - angle2)
    where changeAngle1 = ( angle1 * pi )/ 180

-- Draws the coloured lines using a turtle and the given colour, 
-- it follows the commands in the given string assuming given  
-- initial angle of rotation.
trace :: String -> Float -> Colour -> [ColouredLine]
trace commands a colour 
  = nextLine ((0.0, 0.0), 90) commands [] 
    where 
     nextLine :: TurtleState -> String -> Stack -> [ColouredLine]
     nextLine _ "" _ = []
     nextLine cts ('[' : cs') tStack = nextLine cts cs' (cts : tStack)
     nextLine cts (']' : cs') (e : es) = nextLine e cs' es
     nextLine (initialV, initialA) (c : cs) tStack
       |c == 'F' = (initialV, nextV, colour) : nextLine (nextV, nextA) cs tStack
       |otherwise = nextLine (initialV, nextA) cs tStack
        where
         (nextV, nextA) = move c (initialV, initialA) a
                        
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> Colour -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)
