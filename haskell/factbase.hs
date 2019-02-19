import Data.Map
import Control.Monad.Trans
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Identity
import System.Random
import qualified Data.Set as DSet

type Predicate = String
type Effect = String
type Value = String
type Binding = String
type BindMap = Map Binding Value
type Log = (Int, String)
type Actor = Fact --TODO
type RandMonad = ST.StateT StdGen Identity
type SimMonad = RWST StaticState [Log] SimState RandMonad


data Fact = Fact Predicate [Value] | Binding Predicate [(Binding, Value)] deriving (Show, Eq)
data Rule = Rule [Predicate] [Effect]
data FactBase = FactBase (DSet.Set Fact)
data RuleBase = RuleBase (DSet.Set Rule)
data StaticState = StaticState { maxTurn :: Int }
data SimState = SimState { ssTime :: Int
                         , ssRules :: RuleBase
                         , ssFacts :: FactBase
                         , ssBindings :: BindMap 
                         }

-- FactBase Operations
contains :: Fact -> FactBase -> Bool
contains f (FactBase xs) = f `DSet.member` xs

add :: Fact -> FactBase -> FactBase
add f (FactBase factSet) = FactBase $ f `DSet.insert` factSet

retract :: Fact -> FactBase -> FactBase
retract = undefined

bind :: Fact -> BindMap -> BindMap
bind = undefined

reify :: Fact -> BindMap -> Fact
reify = undefined

-- Initial Values
initialFactSet :: FactBase
initialFactSet = FactBase $ DSet.fromList [ Fact "actor" ["Bob"], Fact "actor" ["Bill"]]

initialRuleSet :: RuleBase
initialRuleSet = RuleBase $ DSet.fromList []

initialBindingMap :: BindMap
initialBindingMap = empty

emptyState :: SimState 
emptyState = SimState 0 initialRuleSet initialFactSet initialBindingMap


-- Rule Ops
bindRules :: SimMonad BindMap
bindRules = undefined

getCustomizedRules :: Maybe Actor -> SimMonad (Maybe [Rule])
getCustomizedRules (Just a) = do
  -- Get rules applicable to the actor
  ruleSet <- gets ssRules
  return $ Just $ getRules ruleSet

getCustomizedRules Nothing = return Nothing


-- Logging
logMsg :: Int -> String -> SimMonad ()
logMsg lvl message = do { tell $ [(lvl, message ++ "\n")]}
logLn a = logMsg 1 a
logDbg a = logMsg (-1) a

-- Utilities
getTime = gets ssTime
incTime = do { s <- get; time <- getTime; put s { ssTime = time + 1} }
reachedMaxTime = do { time <- gets ssTime; maxT <- asks maxTurn; return $ time < maxT }
getRand low high = do
  (val, gen') <- lift $ ST.get >>= \gen -> return $ randomR (low, high) gen
  lift $ ST.put gen'
  return val

getFacts :: FactBase -> [Fact]
getFacts (FactBase f) = toList f
getRules :: RuleBase -> [Rule]
getRules (RuleBase r) = toList r

isActor :: Fact -> Bool
isActor (Fact "actor" _) = True
isActor _ = False

getRandomActor :: SimMonad (Maybe Actor)
getRandomActor = do
  factBase <- gets ssFacts
  let facts = getFacts factBase
  let actors = Prelude.filter isActor facts
  if 0 < length actors then
    do
      randIndex <- getRand 0 $ (length actors) - 1
      logDbg $ "Index is: " ++ (show randIndex) ++ " : " ++ (show $ length actors)
      let actor = actors !! randIndex
      logLn $ "Actor Found: " ++ getActorName actor
      return $ Just actor
  else do { logLn "No Actor Found"; return Nothing }

getActorName :: Actor -> String
getActorName (Fact _ (x:xs)) = x


--add in a random state
runSim :: SimMonad ()
runSim = do
  getTime >>= \t -> logLn $ "----\n Sim Started: " ++ show t

  -- Get character to act
  maybe_actor <- getRandomActor

  -- get active/customized rules ( RuleSet -> FactSet -> Actor -> RuleSet' )
  rules <- getCustomizedRules maybe_actor
  -- run the rules ( RuleSet -> FactSet -> [(Weight, Action)])
  -- select action ([(Weights, Action)] -> Actor -> Action
  -- perform action (Action -> IO (), RuleSet', FactSet', [Actor'])
  
  -- 
  continue <- reachedMaxTime
  if continue then do { incTime; runSim } else return ()

logLvl = 0
main :: IO ()
main = do
  let consts = StaticState 6
  let gen = mkStdGen 25325
  let logPairs = snd $ runIdentity $ ST.evalStateT (evalRWST runSim consts emptyState) gen
  
  -- Print all the written messages, filtered by log level 
  forM_ (Prelude.filter (\(lvl,_) -> lvl >= logLvl) logPairs) (\(lvl, msg) -> putStr msg)
  putStr "----\n Sim Finished\n ----\n"
