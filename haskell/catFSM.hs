{-# LANGUAGE TemplateHaskell #-}
import System.Random
import System.IO
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Identity
import Control.Lens 
import qualified Data.Map as DMap
import Data.List

-- Settings
logPriority = 0
maxTurns = 10

-- Data
type Log = (Int, String)
type RandMonad = ST.StateT StdGen IO
type CatMonad = RWST String [Log] CatState RandMonad
type Transitions = DMap.Map Cat_Activity [Cat_Activity]

data Cat_Activity = Sleeping | Purring | Cleaning | Crapping | Eating | Meowing
                  | Scratching | Hissing | Stretching | Watching deriving (Show, Eq, Ord)

data CatState = CatState { _hunger :: Float
                         , _tired :: Float
                         , _dirty :: Float
                         , _happy :: Float
                         , _readyToCrap :: Float
                         , _currentState :: Cat_Activity
                         , _stepCount :: Int} deriving (Show, Eq)

makeLenses ''CatState

-- Utility calculations of Cat_Activity -> CatState fields
assessUtility :: CatState -> Cat_Activity -> Float
assessUtility state Sleeping = 1.0
assessUtility state Purring = 0.5
assessUtility state Cleaning = 0.4
assessUtility state Crapping = 0.6
assessUtility state Eating = 0.8
assessUtility state Meowing = 0.2
assessUtility state Scratching = 0.2
assessUtility state Hissing = 0.3
assessUtility state Stretching = 0.4
assessUtility state Watching = 0.7



-- Data Setup
initCatState = CatState 0.0 0.0 0.0 0.0 0.0 Sleeping 0
incrementables = [hunger, tired, dirty, happy, readyToCrap]

allCatTransitions :: Transitions
allCatTransitions = DMap.fromList [  (Sleeping, [Hissing, Meowing, Cleaning, Purring, Stretching])
                                  , (Purring , [Cleaning, Meowing, Stretching])
                                  , (Cleaning, [Sleeping, Meowing])
                                  , (Crapping, [Cleaning, Sleeping])
                                  , (Eating, [Sleeping, Purring, Cleaning, Crapping])
                                  , (Meowing, [Scratching, Hissing, Stretching, Watching])
                                  , (Scratching, [Hissing, Sleeping, Purring, Crapping])
                                  , (Hissing, [Crapping, Stretching, Watching])
                                  , (Stretching, [Sleeping, Purring, Cleaning, Meowing])
                                  , (Watching, [Sleeping, Purring, Cleaning, Crapping, Eating, Meowing])
                                  ]

-- General Functions

-- Update all the incrementable and tracked desires of the cat by a random amount
incrementDesires :: Float -> CatMonad ()
incrementDesires amnt = do
  gen <- splitGen
  let amnts = take 5 $ randomRs (0.0, amnt) gen
  state <- get
  let mods = fmap (\(target,val) -> over target (+ val)) $ zip incrementables amnts
  put $ foldl (\memo func -> func memo) state mods
  state' <- get
  tell $ [(0, show state' ++ "\n")]

-- Calculate utilities
calcUtilities :: [Cat_Activity] -> CatState -> [(Cat_Activity, Float)]
calcUtilities activities state = result
  where utils = fmap (assessUtility state) activities
        result = zip activities utils

-- follow random paths from the transition system
selectActivity :: StdGen -> CatState -> [Cat_Activity] -> Maybe Cat_Activity
selectActivity gen state xs = do
  let utilities = sortBy (\(_,q) (_,w) -> compare q w) $ calcUtilities xs state
  let best = take 3 utilities
  let position = randListPos gen best
  if length best > 0
    then return $ fst $ best !! position
    else Nothing

-- assess then trigger action updates
act :: CatMonad ()
act = do
  cState <- get
  gen <- splitGen
  let current = view currentState cState
  let m_nextActivity = (DMap.lookup current allCatTransitions) >>= (selectActivity gen cState)
  case m_nextActivity of
    Just x -> put $ switchActivity x cState
    Nothing -> put $ switchActivity Sleeping cState
  
-- A Single step of the simulation
simStep :: CatMonad ()
simStep = do
  val <- gets $ view currentState
  liftIO $ putStrLn $ "The Cat is " ++ show val
  --
  act
  --
  incrementDesires 0.1
  newCount <- incCount
  if newCount < maxTurns
    then simStep
    else return ()

-- Bookend the sim 
runSim :: CatMonad ()
runSim = do
  liftIO $ putStrLn "---- Testing Cat FSM ----"
  simStep
  liftIO $ putStrLn "---- Finished ----"

----------
-- MAIN
----------
main :: IO ()
main = do
  let gen = mkStdGen 25325
  logged <- ST.evalStateT (evalRWST runSim "Nothing" initCatState) gen
  putStrLn "----------"
  --Print the logged output after the sim has finished
  mapM_ (putStrLn . snd) $ filter (\x -> logPriority <= fst x) $ snd logged
----------

-- UTILS

--increment the step count in the main state 
incCount :: CatMonad Int
incCount = do
  curState <- get
  count <- gets $ view stepCount
  put $ over (stepCount) ( + 1) curState
  return $ count + 1

-- update based on a transition
switchActivity :: Cat_Activity -> CatState -> CatState
switchActivity newAct cState = set currentState newAct cState

-- split off a random generator for use
splitGen :: CatMonad StdGen
splitGen = do
  gen <- lift $ ST.get
  let (gen', gen'') = split gen
  lift $ ST.put gen'
  return gen''

randListPos :: StdGen -> [a] -> Int
randListPos g xs = p
  where len = length xs
        (p, g') = randomR (0, len-1) g
