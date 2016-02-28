module Unassesed3 where

import Data.List

data Ustaff = Teaching EmpData RSection Courses | Support EmpData Stype
              deriving (Eq, Ord, Show)

data Sex = M | F
         deriving (Eq,Ord,Show)

type EmpData = (Name, Sex, DOB, Salary)
type Name = String 
type DOB = (Int, Int, Int)
type Salary = Int

type RSection = String
type Courses = [Int]
type Stype = String
type Course = Int
type Database = [Ustaff]

totalStaff :: Database -> Int 
totalStaff d = sum [1 | a <- d]

nameOfT :: Database -> Course -> Name 
nameOfT db c = filter (== (Teaching)) (db) 
