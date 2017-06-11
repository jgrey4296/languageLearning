module LogUtils where

import Control.Monad.Trans
import Control.Monad.Trans.RWS as RW

data LogLevel = Info | Problem | Critical deriving (Show, Eq, Ord)
type Log = (LogLevel, String)
type LogStream = [Log]

class PrettyPrint a where
  ppr :: a -> String

logI :: (Monad m) => String -> RW.RWST a LogStream b m ()
logI x = RW.tell $ [(Info, x)]

fsmP :: MonadIO m => String -> m ()
fsmP x = liftIO $ putStrLn x

