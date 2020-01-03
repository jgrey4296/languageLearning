{-# LANGUAGE ScopedTypeVariables  #-}
module RandUtils where

import System.Random
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State as ST

-- split off a random generator for use
splitGen :: (Monad a) => ST.StateT StdGen a StdGen
splitGen = do
  gen :: StdGen <- ST.get
  let (gen', gen'') = split gen
  ST.put gen'
  return gen''

randListPos :: StdGen -> [a] -> Int
randListPos g xs = p
  where len = length xs
        (p, g') = randomR (0, len-1) g

randPair :: Random a => (a, a) -> StdGen -> (a, a)
randPair bounds gen = xyTuple
  where [x,y] = take 2 $ randomRs bounds gen
        xyTuple = (x,y)
