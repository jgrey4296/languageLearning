-- from https://wiki.haskell.org/Real_World_Applications/Event_Driven_Applications
import Text.Read

data Domain = Domain Int deriving (Eq, Show)

data Event = EventAdd Int | EventExit deriving (Eq, Show)

dmUpdate :: Domain -> Maybe Event -> (Domain, [Maybe Event])
dmUpdate (Domain v) (Just (EventAdd a)) = (Domain (v + a), newEvents)
  where newEvents = if a > 5 then [Just $ EventAdd 2] else []
dmUpdate dm _ = (dm, [])

uiUpdate :: Domain -> IO [Maybe Event]
uiUpdate (Domain v) = do
  putStrLn $ "Value is now: " ++ show v
  line <- getLine
  let event :: Maybe Event = EventAdd <$> readMaybe line
  if event /= Nothing
    then return [event]
    else return [Just EventExit]

--todo: rewrite to be a RWST r w Domain StateT [Maybe Event] StateT StdGen Identity
--ie: Reading/Writing with a Domain State, carrying the state of an event list, able to go random
run :: Domain -> [Maybe Event] -> IO ()
run dm [] = do
  maybeEvents <- uiUpdate dm
  run dm maybeEvents
run _ (Just EventExit:_) = return ()
run dm (e:es) = run updatedDm updatedEvents
  where (updatedDm, newEvents) = dmUpdate dm e
        updatedEvents = newEvents ++ es

main = run (Domain 0) []
