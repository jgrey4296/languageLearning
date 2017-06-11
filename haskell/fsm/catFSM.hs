{-# LANGUAGE TemplateHaskell #-}
import FSM
import LogUtils

import System.Random
import Data.Map as DMap
import Control.Monad.Trans.State as ST

-- FSM Model of a Cat
-- FSM_State         := The Potential States the FSM can be in.
-- EntityState       := The data of the entity using the FSM, to make decisions on
-- assessUtility     := Functions to consume entity state and FSM action to determine the utility of next move
-- FSM_Events        := The edges between FSM_States

-- incrementables, incrementDesires := The auto-update entity variables per step

-- Settings
logPriority = Info
maxTurns = 10

------------------------------
-- FSM DATA SETUP
------------------------------
-- Data the entity holds as two maps, one for floats, one for strings
initStateF = DMap.fromList [ ("a", 2.0), ("b", 3.0) ]
initStateS = DMap.fromList [ ("emotion", "happy"), ("food","empty") ]


-- Utility calculations of Current Entity State + potential next FSM State
-- todo: make it to also include the stream of previous actions?
assessUtility :: UtilityCalc
assessUtility es fs = 1.0

-- Conditions on transitions:
stateCondition :: TransitionPredicate
stateCondition fm sm state = True

-- States and their transitions
fsm_Events :: Transitions
fsm_Events = DMap.fromList [ tp "A B C", tp "B D", tp "C D", tp "D A"]
  where tp x = let (a:bs) = words x in (fsms a, fmap (\x -> ts x) bs)

-- The initial State of the FSM
startState = fsms "A"

-- The starting entity state
initialState :: EntityState
initialState = EntityState  (initStateF) (initStateS) 0 startState

-- Values of the float state to auto increment
incrementables :: [String] -- todo [(String, Float -> Float)]
incrementables = []
-- todo: add auto-mod of strVals

-- Wraps unchanging elements together
initialEnvironment :: FSM_Environment
initialEnvironment = FSMEnv fsm_Events incrementables assessUtility stateCondition

-- ==============================

----------
-- MAIN
----------
main :: IO ()
main = do
  let gen = mkStdGen 23521235235
  -- Run the State machine:
  logged <- runFSM initialEnvironment initialState gen
  putStrLn "----------"
  --Print the logged output after the sim has finished
  mapM_ (putStrLn . snd) $ Prelude.filter (\x -> logPriority <= fst x) $ logged
----------
