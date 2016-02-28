import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp x
  = fromJust.lookup x

states :: LTS -> [State]
states (((a,b),c):ts) 
  = nub(stateHelper a b ts)
  where
    stateHelper a b (((a',b'),c'):xs) = (a:b:stateHelper a' b' xs)
    stateHelper _ _ _ = []

transitions :: State -> LTS -> [Transition]
transitions a lts
  = [t | t@((s,b),c) <- lts, s == a]

alphabet :: LTS -> Alphabet
alphabet 
  = nub.map (\((a,b),c) -> c) 

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions (Prefix id process)
  = nub(id : actions process)
actions (Choice l@(p:ps))
  |l == [] = []
  |otherwise = nub(actions p ++ actions (Choice ps))
actions a 
  = []

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts [] [] = True 
accepts [] _ 
  = True
accepts _ [] 
  = False 
accepts ids lpds@(p:pds)
  = accepts'' (snd p) lpds ids
--  where
--  accepts' :: ProcessDef -> [ProcessDef] -> [Id] 
--  accepts' (id,STOP) l 
--    = []  
--  accepts' (id,(Ref r)) l
--    = accepts' (r,lookUp r l) l 
--  accepts' (id,(Prefix id' p)) l
--    = id' : accepts' (id,p) l 
--  accepts' (id,(Choice (p:ps))) l
--    = []

accepts'' :: Process -> [ProcessDef] -> [Id] -> Bool
accepts'' (Ref r) lpds ids
  = accepts'' (lookUp r lpds) lpds ids
accepts'' (Prefix i p) lpds (id : ids) 
  | i == id   = accepts'' p lpds ids
  | otherwise = False 
accepts'' (Choice (p:ps)) lpds ids
  | checkChoice p lpds ids  = accepts'' p lpds (ids)
  | otherwise = accepts'' (Choice ps) lpds ids
accepts'' (STOP) ldps 
accepts'' _ _ _ = False 

checkChoice p lpds ids
  | = accepts'' p lpds (ids\\acts)
      

------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions
  = undefined

pruneTransitions :: [Transition] -> LTS
pruneTransitions 
  = undefined

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose 
  = undefined

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])
maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

