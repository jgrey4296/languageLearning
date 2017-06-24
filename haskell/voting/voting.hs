{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad
import System.Random
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T

type Position = [Float]
type Tally = (Position, Int)
type VoteAggregation = [Tally]
type RandMonad = StateT StdGen IO
type ElectionM = StateT VoteAggregation RandMonad


--Without the sqrt when just comparing
cDistance :: Position -> Position -> Float
cDistance as bs = result
  where result = foldl (\m (a,b) -> m + ((a - b) ** 2)) 0.0 $ zip as bs

distance x = sqrt . cDistance x

nearestTo :: Position -> [Position] -> Int
nearestTo pos options = result
  where distances = fmap (\x -> cDistance pos x) options
        result = snd . head . L.sort $ zip distances [1..]


simpleElection :: Int -> Int -> Int -> ElectionM VoteAggregation
simpleElection numVoters numReps vectorSize = do
  reps <- lift $ replicateM numReps $ createVector vectorSize
  voters <- lift $ replicateM numVoters $ createVector vectorSize
  sortVotes reps voters
  voteTallies <- get
  return voteTallies

sortVotes :: [Position] -> [Position] -> ElectionM ()
sortVotes reps voters = do
  --initialise
  put $ zip reps $ take (length reps) $ repeat 0
  let incVoteBound = incVote reps
  mapM_ (\x -> incVoteBound x) voters
  
incVote :: [Position] -> Position -> ElectionM ()
incVote positions voter = do
  let votedFor = nearestTo voter positions
  modify $ incrementForI votedFor 1
  
incrementForI :: Int -> Int -> VoteAggregation -> VoteAggregation
incrementForI pos amnt aggregate = result
  where initial | pos == 0 = []
                | pos > 0 = take (pos - 1) aggregate
        (patt, count) = head . drop (pos - 1) $ aggregate
        rest = drop pos aggregate
        result = initial ++ [(patt, count + amnt)] ++ rest

printResults :: VoteAggregation -> IO ()
printResults results = do
  let alphabet = fmap T.singleton ['a'..'z']
      addSuffixes xs n = fmap (\x -> x <> (T.pack $ show n)) xs <> addSuffixes xs (n+1)
      infiniteSupply = addSuffixes alphabet (0 :: Integer)
      zipped = fmap (\(s, (_, c)) -> (c, s)) $ L.zip infiniteSupply results
  forM_ (reverse . L.sort $  zipped) (\(c, s) -> putStrLn $ T.unpack $ s <> (T.pack ": ") <> (T.pack $ show c))

createVector :: Int -> RandMonad Position
createVector size = do
  gen <- splitGen
  let vec = take size $ randomRs ((-1.0), 1.0) gen
  return vec
  
splitGen :: RandMonad StdGen
splitGen = do
  gen <- get
  let (gen', gen'') = split gen
  put gen''
  return gen'

runElection :: Int -> Int -> Int -> IO ()
runElection numVoters numReps vecSize = do
  print "Starting Election"
  print $ "Voters : " ++ (show numVoters) ++ " Reps: " ++ (show numReps)
  print $ "Vector Size: " ++ show vecSize
  result <- evalStateT (evalStateT (simpleElection numVoters numReps vecSize) []) (mkStdGen 235342)
  if length result /= numReps
    then error "Result Mismatch"
    else printResults result
  print "Finished"

main = runElection 1000 20 10
