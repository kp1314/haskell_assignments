module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\#"

-- Checks to see if a certain string matches any in the given list 
-- of tuples and delivers a list of the corresponding items in the 
-- tuples.
lookUp :: String -> [(String, a)] -> [a]
lookUp string1 []
    = []
lookUp string1 ((string2, z) : ys)
    = if string1 == string2 
      then (z : lookUp string1 ys) 
      else lookUp string1 ys

-- Takes the list of separators from a given string and the string 
-- itself, then removes the separators from the string leaving a 
-- outputing a tuple consisting of separators and a list of words 
-- with null strings in places where separators once were.
split :: [Char] -> String -> (String, [String])
split _ "" = ([] , [""])
split separators (y:ys)
	|elem y separators = (y : rest, "" : letters)
	|otherwise = (rest, (y:x):xs)
		     where (rest, letters@(x:xs)) = split separators ys 

-- Merges the sring of separators and the list of words inside the tuple
-- outputted by split to give a list consisting of words with separators 
-- in the order of the original scentence imputted into split.
combine :: String -> [String] -> [String]
combine [] words = words
combine (seperator : seperators) (word : words) =
	word : (seperator : []) : (combine seperators words)

-- Takes a string of information and forms a list of tuples showing the 
-- keyword and it's corresponding definition.
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs (y : rest) 
  = (b, concat (combine spaces bs)) : getKeywordDefs rest
      where ((space:spaces), (b:bs)) = split " " y

-- Takes the inputted text and info file and applies the replaceWord function and 
-- combineText function to them to produce a string with definitions in place on 
-- their keywords in the original text file.
expand :: FileContents -> FileContents -> FileContents
expand "" _ = []
expand text info
  = concat $ replaceWord (combineText text) $ x 
      where x = getKeywordDefs $ snd (split "\n" info)

-- Combines the result of splitting the inputted text 
combineText :: String -> [String]
combineText seps 
  = combine x y
      where (x, y) = split separators seps

-- Replaces the keywords in the combined text with their definitions and outputs 
-- an unconcatenated list of the words and separators in the right order
replaceWord :: [String] -> KeywordDefs -> [String]
replaceWord [] a = []
replaceWord (firstword : words) a
	|lookUp firstword a == [] = (firstword : replaceWord words a)
	|otherwise = (head (lookUp firstword a) : replaceWord words a)

main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")

