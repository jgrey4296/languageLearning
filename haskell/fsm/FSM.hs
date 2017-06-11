{-# LANGUAGE TemplateHaskell #-}
module FSM where

import RandUtils
import NumUtils
import LogUtils

import System.Random
import System.IO
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Identity
import Control.Lens 
import qualified Data.Map as DMap
import Data.List
import Data.String as S
import qualified Data.Set as Set

-- FSM_State         := The Potential States the FSM can be in.
-- EntityState       := The data of the entity using the FSM, to make decisions on
-- assessUtility     := Functions to consume entity state and FSM action to determine the utility of next move
-- FSM_Events        := The edges between FSM_States

-- incrementables, incrementDesires := The auto-update entity variables per step

-- Settings
maxTurns = 10


-- Data
type FloatMap = DMap.Map String Float
type StringMap = DMap.Map String String
type RandMonad = ST.StateT StdGen IO
type FSMMonad = RWST FSM_Environment LogStream EntityState RandMonad
type UtilityCalc = EntityState -> Transition -> Float
type TransitionPredicate = FloatMap -> StringMap -> Transition -> Bool

-- States of the FSM - to be loaded from a text file
data FSM_State = FSMState String deriving (Show, Eq, Ord)


data FSM_Environment = FSMEnv { fsmE_transitions :: Transitions
                              , fsmE_incrementables :: [String]
                              , fsmE_utilityCalc :: UtilityCalc
                              , fsmE_transitionPredicate :: TransitionPredicate }

data Transition = Transition { transEffects :: EntityState -> EntityState
                             , transTarget :: FSM_State }
                  
type Transitions = DMap.Map FSM_State [Transition]

-- The State of the FSM entity/world
data EntityState = EntityState { _entFloatVals :: FloatMap
                               , _entStrVals :: StringMap
                               , _stepCount :: Int
                               , _currentState :: FSM_State } deriving (Show, Eq)

makeLenses ''EntityState

-- Class Instances for printing:
instance PrettyPrint FSM_State where
  ppr (FSMState s) = "{ " ++ s ++ " }"

instance PrettyPrint Transition where
  ppr t = target
    where (FSMState target) = transTarget t

instance PrettyPrint Transitions where
  ppr transitions
    | DMap.size transitions == 0 = "[]"
    | otherwise = combined
    where transFold = foldl (\m t -> m ++ ppr t ++ " | " ) " --> "
          allFold = DMap.foldlWithKey (\m k ts -> m ++ "\n " ++ (ppr k) ++ transFold ts)
          combined = allFold " ==>  " transitions

instance PrettyPrint EntityState where
  ppr eState = result
    where fState = view entFloatVals eState
          sState = view entStrVals eState
          fStateStr = DMap.foldlWithKey (\m k v -> m ++ k ++ " : " ++ show v ++ ", ") "FloatMap: " fState
          sStateStr = DMap.foldlWithKey (\m k v -> m ++ k ++ " : " ++ v ++ ", ") "StringMap: " sState
          result = "EntityState: \n\t" ++ fStateStr ++ "\n\t" ++ sStateStr

set_ppr :: Set.Set FSM_State -> String
set_ppr set = result
  where result = Set.foldl (\m v -> m ++ ppr v) ":: " set

-- Data Constructors from strings:
instance IsString FSM_State where
  fromString s = FSMState s

fsms :: String -> FSM_State
fsms = fromString

instance IsString Transition where
  fromString s = Transition id $ fsms s

ts :: String -> Transition
ts = fromString


-- General Functions

-- Update all the incrementable and tracked desires of the cat by a random amount
incrementDesires :: Float -> FSMMonad ()
incrementDesires amnt = do
  gen <- lift $ splitGen
  incrementables <- asks fsmE_incrementables
  let amnts = take 5 $ randomRs (0.0, amnt) gen
  --state <- gets
  -- todo: let each variable have its own curve
  let incrementAmnts = zip incrementables amnts
      modMap = fmap (\(target,val) -> DMap.update (\x -> Just $ guardFloat id $ x + val) target)
      mods = modMap incrementAmnts
      mapUpdate mp = foldl (\st mod -> mod st) mp mods
  modify $ over entFloatVals mapUpdate
  state' <- get
  logI $ ppr state' ++ "\n"

-- Calculate utilities
calcUtilities :: UtilityCalc -> [Transition] -> EntityState -> [(Transition, Float)]
calcUtilities calc activities state = result
  where utils = fmap (calc state) activities
        result = zip activities utils

-- follow random paths from the transition system
selectActivity :: StdGen -> UtilityCalc -> EntityState -> [Transition] -> Maybe Transition
selectActivity gen calc state xs = do
  let utilities = sortBy (\(_,q) (_,w) -> compare q w) $ calcUtilities calc xs state
      best = take 3 utilities
      position = randListPos gen best
  if length best > 0
    then return $ fst $ best !! position
    else Nothing


-- assess then trigger action updates
act :: FSMMonad ()
act = do
  entityState <- get
  gen <- lift $ splitGen
  allTransitions <- asks fsmE_transitions
  utilityCalcs <- asks fsmE_utilityCalc
  transitionPredicate <- asks fsmE_transitionPredicate
  let current = view currentState entityState
      fState = view entFloatVals entityState
      sState = view entStrVals entityState
      exclusionCheck = transitionPredicate fState sState
      -- retrieve transitions:
      m_potentialEvents = DMap.lookup current allTransitions
      -- filter by world state:
      m_realisticEvents = fmap (\x -> Prelude.filter exclusionCheck x) m_potentialEvents
      -- now select one activity:
      m_nextActivity = m_realisticEvents >>= (selectActivity gen utilityCalcs entityState)
  -- now move to that activity
  case m_nextActivity of
    Just x -> moveToNewState x
    Nothing -> return () -- todo: move to a random new state
  
-- A Single step of the simulation
simStep :: FSMMonad ()
simStep = do
  val <- gets $ view currentState
  logI $ "Current State: " ++ ppr val
  fsmP $ "The Cat is " ++ ppr val -- todo: improve printing of state
  --
  act
  --
  incrementDesires 0.1
  newCount <- incCount
  if newCount < maxTurns
    then simStep
    else return ()


-- Bookend the sim 
runSim :: FSMMonad ()
runSim = do
  fsmP "---- Testing Cat FSM ----"
  logTransitions
  verifyFSM
  simStep
  fsmP "---- Finished ----"

runFSM :: FSM_Environment -> EntityState -> StdGen -> IO LogStream
runFSM e s g = do
  logged <- ST.evalStateT (evalRWST runSim e s) g
  return $ snd logged
  


-- UTILS

--increment the step count in the main state 
incCount :: FSMMonad Int
incCount = do
  curState <- get
  count <- gets $ view stepCount
  put $ over (stepCount) ( + 1) curState
  return $ count + 1

-- update based on a transition
moveToNewState :: Transition -> FSMMonad ()
moveToNewState trans = do
  entState <- get
  --todo: enact the effects
  -- then set the new currenstate to the target
  modify $ set currentState (transTarget trans)

-- Logging
--logTransitions :: WriterT m => m LogStream ()
logTransitions = do
  trans <- asks fsmE_transitions
  logI $ "Available Transitions: " ++ ppr trans



--------------------
-- Verification
--------------------

verifyFSM :: FSMMonad ()
verifyFSM = do
  verifyStartState
  verifyInputsAndOutputs

verifyStartState :: FSMMonad ()
verifyStartState = do
  startState <- gets $ view currentState
  allTransitions <- asks fsmE_transitions
  let startStateIsValid = startState `DMap.member` allTransitions
  if not startStateIsValid
    then fail "Start State is disconnected"
    else return ()

verifyInputsAndOutputs :: FSMMonad ()
verifyInputsAndOutputs = do
  allTransitions <- asks fsmE_transitions
  let allStates :: Set.Set FSM_State = Set.fromList $ DMap.keys allTransitions
      allTransitionElems :: [Transition] = foldl1 (++) $ DMap.elems allTransitions
      allTargets :: Set.Set FSM_State =  Set.fromList $ fmap (\x -> transTarget x) allTransitionElems
  logI $ "State Set: " ++ set_ppr allStates
  logI $ "Target Set: " ++ set_ppr allTargets
  let diff = Set.unions [Set.difference allStates allTargets, Set.difference allTargets allStates]
  if not $ Set.null diff
    then fail $ "There are states that do not match or are undefined: " ++ set_ppr diff
    else return ()
