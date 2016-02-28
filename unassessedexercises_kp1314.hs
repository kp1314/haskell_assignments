module Unassessed where

import Data.Char (ord, chr)

numBarrels :: Int -> Int -> Int
--divides number od litres by barrels giving the quotient
numBarrels a b 
	| a `mod` b == 0 = div a b
	| otherwise = (div a b) + 1

