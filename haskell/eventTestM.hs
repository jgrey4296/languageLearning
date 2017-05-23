-- from https://wiki.haskell.org/Real_World_Applications/Event_Driven_Applications
import qualified Text.Read as TR
import Data.Map
import Control.Monad.Trans as CMT
import Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.State as ST
import Control.Monad.Identity
import System.Random
import qualified Data.Set as DSet

data InputType = User | Auto deriving (Show, Eq)
data Domain = Domain { dVal :: Int } deriving (Eq, Show)
data Event = EventAdd Int InputType | EventExit deriving (Eq, Show)

type RandM = ST.StateT StdGen IO
type EventM a  = ST.StateT [Maybe Event] a
type DomainM = RWST String String Domain (EventM RandM)

userEvent x = EventAdd x User


dmUpdate :: DomainM (Maybe Event)
dmUpdate = do
  event <- retrieveEvent
  liftIO $ putStrLn $ "Retrieved: " ++ show event
  applyEvent event
  

retrieveEvent :: DomainM (Maybe Event)
retrieveEvent = do
  events <- CMT.lift $ ST.get
  if length events > 0
    then do
    let ev = head events
        es = tail events 
    CMT.lift $ ST.put es
    return ev
    else return Nothing

  
applyEvent :: Maybe Event -> DomainM (Maybe Event)
applyEvent Nothing = return Nothing
applyEvent (Just (EventAdd a b)) = do
  dom <- get
  let dv = dVal dom
  put $ dom { dVal = dv + a }
  if a > 5
    then CMT.lift $ ST.modify (++[Just $ EventAdd 3 Auto])
    else return ()
  return $ Just $ EventAdd a b

applyEvent (Just EventExit) = return $ Just EventExit
      
      
uiUpdate :: Maybe Event -> DomainM ()
uiUpdate ev = do
  v <- gets dVal
  tell $ "Value is now: " ++ show v ++ " from: " ++ show ev ++ "\n"
  liftIO $ putStrLn $ "Value is now: " ++ show v
  line <- liftIO getLine
  let event :: Maybe Event = userEvent <$> TR.readMaybe line
  if event /= Nothing
    then CMT.lift $ ST.modify (++[event])
    else CMT.lift $ ST.modify (++[Just EventExit])

--todo: rewrite to be a RWST r w Domain StateT [Maybe Event] StateT StdGen Identity
--ie: Reading/Writing with a Domain State, carrying the state of an event list, able to go random
runDomain :: DomainM ()
runDomain = do
  lastEvent <- dmUpdate
  uiUpdate lastEvent
  if lastEvent == (Just $ EventExit)
    then return ()
    else runDomain

main :: IO ()
main = do
  let r = "Reader"
  let gen = mkStdGen 25325
  let ranRWST = evalRWST runDomain r (Domain 0)
  let ranEvents = ST.evalStateT ranRWST []
  (_, writtenLog) <- ST.evalStateT ranEvents gen

  putStrLn writtenLog
