import Data.Map as M
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Random

--- DATA Declarations

--The individuals that can perform actions
data Actor = Actor { actorName :: String
                   , actorState :: Map String Float
                   , actorKnownActions :: [Action] } deriving (Show, Eq)

-- Effects of an action
-- Effect StateKey FloatMod
data Effect = Effect String Float deriving (Show, Eq)

-- Comparisons for conditions.
data Comp = LT | GT | EQ | NEQ deriving (Show, Eq)

-- Conditions
-- ActorStateCondition (Actor, Action, Value, Relation)
-- PriorActionCondition (Actor, Action, TimeFrame, Relation)
data Condition = ActorStateCondition String String Float Comp
               | PriorActionCondtion String String Int deriving (Show, Eq)

-- Actions performed, with consequences for the actor and target
data Action = Action { actionName :: String
                     , actionConditions :: [Condition]
                     , actorEffects :: [Effect]
                     , targetEffects :: [Effect]
                     } deriving (Show, Eq)

-- PerformedAction Actor Target ActionName Timeframe
data PerformedAction = PerformedAction String String String Int

-- a Preferred direction of movement of a state.
data Direction a = Up | Down | Stable | Target a | Avoid a deriving (Show, Eq)
data EffectDirection = EffectDirection String (Direction Float) deriving (Show, Eq)

-- An ordered list of preference bins, from most -> least
-- eg: [[("Fed", Target 1.0)], [("Die", Avoid 1.0)]]
data Preferences = Preferences [[EffectDirection]]

--- FUNCTIONS
selectAction :: Actor -> Preferences -> [PerformedAction] -> Maybe Action
selectAction = undefined

performAction :: Actor -> Actor -> Action -> (Actor, Actor)
performAction = undefined

-- initial values
initialActorState :: Map String Float
initialActorState = M.empty

initialActors :: ActorMap
initialActors = M.fromList [("Bob", Actor "Bob" initialActorState [])]
  

-- Monad Definitions
type ActorMap = Map String Actor
type RandMonad = StateT StdGen IO
type CombatMonad = StateT ActorMap RandMonad

actionPerformance :: CombatMonad ()
actionPerformance actor action target = do
  liftIO $ (actorName actor) ++ " did " ++ (actionName action) ++ " to " ++ (actorName target)
  let (a', t') = performAction actor target action
  == update monad state
  return ()



stepTurn :: Int -> CombatMonad ()
stepTurn n = do
  liftIO $ putStrLn $ "Turn : " ++ show n
  -- determine action order
  -- in order, select action and perform it
  
  if n > 10
    then return ()
    else do stepTurn (n + 1)



main :: IO ()
main = do
  let gen = mkStdGen 253242
  evalStateT (evalStateT (stepTurn 0) initialActors  ) gen
