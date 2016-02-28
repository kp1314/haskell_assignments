module Quadratic where

import Data.Char (ord, chr)

quad :: Int -> Int -> Int -> Int -> Int
-- Returns evaluated quadratic expression.
quad a b c x
  = a*x^2 + b*x + c

quadIsZero :: Int -> Int -> Int -> Int -> Bool
-- Returns True if a quadratic expression evaluates to zero;
-- False otherwise
quadIsZero a b c x
  = quad a b c x == 0

quadraticSolver :: Float -> Float -> Float -> (Float,Float)
-- Returns the two roots of a quadratic equation with
-- coefficients a, b, c
quadraticSolver a b c = (x, t)
     where u = sqrt(b^2-(4*a*c))
           x = (-b+u)/(2*a)
           t = (-b-u)/(2*a)
     
realRoots :: Float -> Float -> Float -> Bool
-- Returns True if the quadratic equation has real roots;
-- False otherwise
realRoots a b c
 = b^2 - 4*a*c >= 0.0

bigger, smaller :: Int -> Int -> Int
-- Returns first argument if it is larger than the second, the second argument
-- otherwise
bigger a b =
     if a>b then a else b 

-- Opposite of bigger
smaller a b = 
     if a<b then a else b

biggestOf3, smallestOf3 :: Int -> Int -> Int -> Int
-- Returns the largest/smallest of three Ints
biggestOf3 a b c = 
     bigger a (bigger b c)

-- Ditto smallest of three
smallestOf3 a b c =
     smaller a (smaller b c)


isADigit :: Char -> Bool
-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit a = 
      ord a >= ord '0' && ord a <= ord '9'

-- False otherwise
isAlphabetic :: Char -> Bool
-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
isAlphabetic a =
     (ord a >= ord 'a' && ord a <= ord 'z') || (ord a >= ord 'A' && ord a <= ord 'Z')

digitCharToInt :: Char -> Int
-- Returns the integer [0..9] corresponding to the given character.
digitCharToInt a 
	|(ord a >= ord '0' && ord a <= ord '9') = ord a - ord '0'
	|otherwise = error "Not a Valid Character"

toUpperCase :: Char -> Char
-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpperCase a 
	|(ord a >= ord 'a' && ord a <= ord 'z')=chr(ord a - 32)
	|otherwise = a  
