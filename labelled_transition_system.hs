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
lookUp
  = (fromJust .) . lookup

states :: LTS -> [State]
states 
  = nub . states' 
  where 
    states' [] 
      = [] 
    states' (((s1, s2), _) : xs) 
      = s1 : s2 : states' xs   

transitions :: State -> LTS -> [Transition]
transitions state 
  = filter (\((s1, _), _) -> s1 == state) 

alphabet :: LTS -> Alphabet
alphabet 
  = nub . alphabet' 
  where 
    alphabet' [] 
      = [] 
    alphabet' (((_, _), i) : xs)
      = i : alphabet' xs     

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions STOP
  = [] 
actions (Ref _)  
  = [] 
actions (Prefix x p) 
  = nub $ x : actions p
actions (Choice ps)
  = nub $ concatMap actions ps 

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts ids ps 
  = accepts' ids (ps ++ [head ps])
  where   
    accepts' [] _ 
      = True 
    accepts' ids ((_, Ref x) : ps) 
      = accepts' ids ((x, p) : ps) 
      where 
        p = lookUp x ps
    accepts' (id : ids) ((s, Prefix x p) : ps) 
      | id == x   = accepts' ids ((s, p) : ps) 
      | otherwise = False 
    accepts' ids ((s, Choice p) : ps)  
      = any (accepts' ids) ps' 
      where 
        pList = zip (repeat s) p   
        ps'   = map (: ps) pList 
    accepts' _ _ 
      = False 

------------------------------------------------------
-- PART III

composeTransitions :: Transition -> Transition 
                  -> Alphabet -> Alphabet 
                  -> StateMap 
                  -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t), a) ((s', t'), a') alpha alpha' sMap 
  | a == a' 
    = [((start, target1), a)]
  | elem a alpha' && elem a' alpha 
    = [] 
  | elem a' alpha 
    = [((start, target2), a)]
  | elem a alpha' 
    = [((start, target3), a')]   
  | otherwise
    = [((start, target3), a'), ((start, target2), a)]    
    where 
      start   = lookUp (s, s') sMap
      target1 = lookUp (t, t') sMap 
      target2 = lookUp (t, s') sMap 
      target3 = lookUp (s, t') sMap    


pruneTransitions :: [Transition] -> LTS
pruneTransitions ts 
  = nub $ visit 0 []
  where 
    visit x xs 
      | notElem x xs = s ++ concatMap (\((from, to), _) -> visit to (from : xs)) s
      | otherwise    = []  
      where 
        s = transitions x ts         

------------------------------------------------------
-- PART IV

cartesianProduct :: [State] -> [State] -> StateMap
cartesianProduct s1 s2 
  = cartesianProduct' 0 0 0 
  where 
    cartesianProduct' n1 n2 s
      | n1 >= length s1 
        = []
      | n2 < length s2 
        = ((s1 !! n1, s2 !! n2), s) : cartesianProduct' n1 (n2 + 1) (s + 1)
      | n2 >= length s2
        = cartesianProduct' (n1 + 1) 0 s       

compose :: LTS -> LTS -> LTS
compose lts lts' 
  = pruneTransitions 
    $ concatMap (\x -> concatMap 
                (\y -> composeTransitions x y alpha alpha' sMap) (lts' ++ sentinel')) (lts ++ sentinel)
  where 
    alpha     = "$2" : alphabet lts  
    alpha'    = "$1" : alphabet lts' 
    s         = states lts 
    s'        = states lts'
    sentinel  = map (\s -> ((s, 0), "$1")) s 
    sentinel' = map (\s -> ((s, 0), "$2")) s' 
    sMap      = cartesianProduct s s' 



------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS ps 
  = result
  where 
    (_ , _, result) = buildLTS' ps 0 1 []   
    buildLTS' [] now next lts
      = (now, next, lts)   
    buildLTS' ((id, p) : xs) now next lts
      | STOP <- p
        = (now, next, lts) 
      | Ref _ <- p 
        = buildLTS' xs now next lts
      | Prefix x process <- p
        =  buildLTS' ((id, process) : xs) next (next + 1) (((now, next), x) : lts) 
      | Choice [] <- p
        = buildLTS' xs now next lts 
    buildLTS' ((id, Choice (m : ms)) : xs) now next lts
      = buildLTS' ((id, Choice ms) : xs) now next' lts'       
      where 
        (_, next', lts') = buildLTS' [(id, m)] now next lts    


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
