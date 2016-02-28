module Recursion where

-- Precondition on all integers: they're all non-negative.

isPrime :: Int -> Bool
isPrime m
     |m < 2 = False
     |m == 2 = True
     |otherwise = counter 2 
	where counter :: Int -> Bool
	      counter n 
                |n > floor (sqrt (fromIntegral m)) = True
	        |m `mod` n == 0 = False
	        |otherwise = counter (n + 1) 
           
nextPrime :: Int -> Int
nextPrime n
     |n<2 = 2
     |otherwise = addOne (n+1)
        where addOne :: Int -> Int
	      addOne n
	        |isPrime n = n
                |otherwise = addOne(n+1)

modPow :: Int -> Int -> Int -> Int
-- Pre: 1 <= m <= sqrt(maxint)
modPow a b m 
-- m > sqrt((2^63)-1) = error "m too big"
   = (simplify a b) `mod` m
-- we want it to simplify b repeatedly until b<3 (base case) then do modPow as less than 3 is easy to compute
   where simplify :: Int -> Int -> Int
         simplify s t
            |t < 3 = (s^t)
            |t `mod` 2 /= 0 = ((s `mod` m)*(simplify s (t-1))) 
            |t `mod` 2 == 0 = ((simplify s (t `div` 2)) `mod` m)^2           

isCarmichael :: Int -> Bool
isCarmichael n  
--if number is prime it is not a Carmichael number
	|isPrime n = False
	|otherwise = fermatTest (n-1)
	  where fermatTest :: Int -> Bool
		fermatTest m
			|m<2 = True 
			|modPow m n n == m = fermatTest(m-1) 
			|otherwise = False

primeFactors :: Int -> [ Int ]
-- Pre: x >= 1
primeFactors n
	|isPrime n = [n]
	|n == 1 = [n]
	|n`mod`2 == 0 = (2 : primeFactors (n`div`2))
	|otherwise = isFactor 3
	 where isFactor :: Int -> [Int]
	       isFactor x
-- the sign $ lets me dismiss the use of brackets
		      |x > truncate(sqrt $ fromIntegral n) = [n]
		      |n`mod`x == 0 = (x : primeFactors (n`div`x))
		      |otherwise = isFactor (nextPrime x) 

sumDigits :: Int -> Int
sumDigits n 
	|n < 0 = error "Must be non-negative input"
	|n == 0 = 0
-- number 10 helps to break down number and leave units
	|otherwise = n `mod` 10 + (sumDigits (n `div` 10))

sumAllDigits :: [ Int ] -> Int
sumAllDigits (x : xs)
   	|xs == [] = sumDigits x
	|otherwise = sumDigits x + sumAllDigits xs

nextSmithNumber :: Int -> Int
nextSmithNumber s
-- always checks next number above the input (it dosen't matter for less than the first smith number 4)
	|s<4 = 4
	|isPrime (s+1) = nextSmithNumber (s+1)
	|sumAllDigits (primeFactors (s+1)) == sumDigits (s+1) = s+1
	|otherwise = nextSmithNumber (s+1) 
