module Tests where

import IC.TestSuite

import Quadratic

quadTestCases
  = [ (1, 0, 2, (-1)) ==> 3
    , (0, 2, 0, 4)    ==> 8
    , (7, 3, 6, 10)   ==> 736
    ]

quadIsZeroTestCases
  = [ (0, 0, 0, 3) ==> True
    , (4, 5, 1, 2) ==> False
    ]

quadraticSolverTestCases
  = [ (2, (-6), 2) ==> (2.618034, 0.381966)
    ]

realRootsTestCases
  = [ (2, (-6), 2) ==> True
    , (2, 2, 2)    ==> False
    ]

biggerTestCases
  = [ (1, 2) ==> 2
    , (2, 1) ==> 2
    , (2, 2) ==> 2
    ]

smallerTestCases
  = [ (1, 2) ==> 1
    , (2, 1) ==> 1
    , (1, 1) ==> 1
    ]

biggestOf3TestCases
  = [ (1, 2, 3) ==> 3
    , (1, 3, 2) ==> 3
    , (3, 2, 1) ==> 3
    , (3, 3, 3) ==> 3
    ]

smallestOf3TestCases
  = [ (1, 2, 3) ==> 1
    , (1, 3, 2) ==> 1
    , (3, 2, 1) ==> 1
    , (1, 1, 1) ==> 1
    ]

isADigitTestCases
  = [ '0' ==> True
    , '1' ==> True
    , '9' ==> True
    , 'a' ==> False
    , '!' ==> False
    ]

isAlphabeticTestCases
  = [ 'a' ==> True
    , 'm' ==> True
    , 'M' ==> True
    , '!' ==> False
    ]

digitCharToIntTestCases
  = [ '8' ==> 8
    , '0' ==> 0
    , '9' ==> 9
    ]

toUpperCaseTestCases
  = [ 'a' ==> 'A'
    , 'A' ==> 'A'
    , 'z' ==> 'Z'
    ]


allTestCases
  = [ TestCase "quad" (uncurry4 quad)
                      quadTestCases
    , TestCase "quadIsZero" (uncurry4 quadIsZero)
                            quadIsZeroTestCases
    , TestCase "quadraticSolver" (uncurry3 quadraticSolver)
                                 quadraticSolverTestCases
    , TestCase "realRoots" (uncurry3 realRoots)
                           realRootsTestCases
    , TestCase "bigger" (uncurry bigger)
                        biggerTestCases
    , TestCase "smaller" (uncurry smaller)
                         smallerTestCases
    , TestCase "biggestOf3" (uncurry3 biggestOf3)
                            biggestOf3TestCases
    , TestCase "smallestOf3" (uncurry3 smallestOf3)
                             smallestOf3TestCases
    , TestCase "isADigit" isADigit
                          isADigitTestCases
    , TestCase "isAlphabetic" isAlphabetic
                              isAlphabeticTestCases
    , TestCase "digitCharToInt" digitCharToInt
                                digitCharToIntTestCases
    , TestCase "toUpperCase" toUpperCase
                             toUpperCaseTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
