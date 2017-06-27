{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad
import System.Random
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Map as M

type Position = [Float]
type Tally = (Position, Int) -- todo: switch from int count to [Int] to allow for preferences
type Rank = (Position, [Int])
type VoteAggregation = M.Map Position Int
type RandMonad = StateT StdGen IO
type ElectionM = StateT VoteAggregation RandMonad

--Without the sqrt when just comparing
cDistance :: Position -> Position -> Float
cDistance as bs = result
  where result = foldl (\m (a,b) -> m + ((a - b) ** 2)) 0.0 $ zip as bs

distance x = sqrt . cDistance x

-- returns a sorted low->high ranking of positions
rankDistances :: Position -> [Position] -> [Position]
rankDistances pos options = result
  where distances = allDistances pos options
        result = fmap snd $ L.sort $ zip distances options

allDistances :: Position -> [Position] -> [Float]
allDistances pos options = distances
  where distances = fmap (\x -> cDistance pos x) options


-- ##############################
-- ELECTION TYPES
-- ##############################

firstPastThePost :: Int -> Int -> Int -> ElectionM VoteAggregation
firstPastThePost numVoters numReps vectorSize = do
  (reps, voters) <- electionSetup numVoters numReps vectorSize
  -- assign votes by similarity to reps
  let allRanked = fmap (flip rankDistances reps) voters
      topVotes = fmap head allRanked
  tallyVotes reps topVotes
  voteTallies <- get
  return voteTallies

instantRunoff :: Int -> Int -> Int -> Int -> ElectionM VoteAggregation
instantRunoff numRankings numVoters numReps vectorSize = do
  -- setup
  (reps, voters) <- electionSetup numVoters numReps vectorSize
  -- rank
  let allRanked = fmap (flip rankDistances reps) voters
  -- runoff
  recursiveRunoff reps allRanked (numVoters `div` 2)
  voteTallies <- get
  return voteTallies

-- Calc first place, if enough, end. otherwise, recurse
recursiveRunoff :: [Position] -> [[Position]] -> Int -> ElectionM ()
recursiveRunoff reps ranks minVotes = do
  liftIO $ print $ "Running off: " ++ show (length ranks)
  tallyVotes reps $ fmap head ranks
  results <- get
  liftIO $ printResults results

  winnerAmnt <- gets maximum
  liftIO $ print $ "Winner amnt: " ++ show winnerAmnt
  if winnerAmnt >= minVotes
    then return ()
    else setupRecurse reps ranks minVotes

-- Calc last place, remove, run next runoff
setupRecurse :: [Position] -> [[Position]] -> Int -> ElectionM ()
setupRecurse reps ranks minVotes = do
  tallyVotes reps $ fmap last ranks
  minResults <- get
  let (minPos, minAmnt) = getCmpFromMap (\x y -> x < y && x > 0) minResults
      ranks' = fmap (filter (\y -> y /= minPos)) ranks
      ranks'' = filter (\x -> 0 < length x) ranks'
  recursiveRunoff reps ranks'' minVotes


bordaCount :: Int -> Int -> Int -> ElectionM VoteAggregation
bordaCount numVoters numReps vectorSize = do
    (reps, voters) <- electionSetup numVoters numReps vectorSize
    let allRanked = fmap (flip rankDistances reps) voters
        voteTuples = fmap (flip zip [1..]) allRanked
    tallyVoteTuples reps voteTuples
    voteTallies <- get
    return voteTallies


approvalVote :: Int -> Int -> Int -> Float -> ElectionM VoteAggregation
approvalVote numVoters numReps vectorSize approvalDistance = do
  (reps, voters) <- electionSetup numVoters numReps vectorSize
  let dists :: [[(Position, Float)]] = fmap (\x -> zip reps $ allDistances x reps) voters
      approved  = fmap (fmap fst) $ fmap (filter (\(v,d) -> d < (approvalDistance ** 2))) dists
  tallyApprovals reps approved
  voteTallies <- get
  return voteTallies

scoreVote :: Int -> Int -> Int -> ElectionM VoteAggregation
scoreVote numVoters numReps vectorSize = do
  (reps, voters) <- electionSetup numVoters numReps vectorSize
  let dists :: [[(Position, Float)]] = fmap (\x -> zip reps $ allDistances x reps) voters
      maxDistance = vectorSize*2*2
      scores :: [[(Position, Int)]] = fmap (\x -> scaleScore x maxDistance numReps) dists
  tallyVoteTuples reps scores
  voteTallies <- get
  return voteTallies

scaleScore :: Integral a => [(Position, Float)] -> Int -> Int -> [(Position, a)]
scaleScore dists maxDistance numReps = result
  where f_maxDistance = fromIntegral maxDistance
        f_numReps = fromIntegral numReps
        result = fmap (\(p,fl) -> (p, round $ linearScale fl 0.0 f_maxDistance f_numReps 0.0)) dists 


linearScale :: Fractional a => a -> a -> a -> a-> a -> a
linearScale x inMin inMax outMin outMax = result
  where outRange = outMax - outMin
        inRange = inMax - inMin
        xFloor = x - inMin
        reduced = xFloor / inRange
        result = outMin + (reduced * outRange)

-- Utilities:

--Init the reps, voters for an election
electionSetup :: Int -> Int -> Int -> ElectionM ([Position], [Position])
electionSetup numVoters numReps vectorSize = do
  reps <- lift $ replicateM numReps $ createVector vectorSize
  voters <- lift $ replicateM numVoters $ createVector vectorSize
  return (reps, voters)
  
--For each rep, count the votes for it
tallyVotes :: [Position] -> [Position] -> ElectionM ()
tallyVotes reps votes = do
  put $ M.fromList $ zip reps $ repeat 0
  mapM_ (\x -> incVote x 1) votes

tallyVoteTuples :: [Position] -> [[(Position, Int)]] -> ElectionM ()
tallyVoteTuples reps votes = do
  put $ M.fromList $ zip reps $ repeat 0
  mapM_ (\rankings -> mapM_ (\(pos, amnt) -> incVote pos amnt) rankings) votes

tallyApprovals :: [Position] -> [[Position]] -> ElectionM ()
tallyApprovals reps votes = do
  put $ M.fromList $ zip reps $ repeat 0
  mapM_ (\approved -> mapM_ (\x -> incVote x 1) approved) votes

incVote :: Position -> Int -> ElectionM ()
incVote vote amnt  = do
  modify $ M.adjust (+ amnt) vote

printResults :: VoteAggregation -> IO ()
printResults results = do
  let alphabet = fmap T.singleton ['a'..'z']
      addSuffixes xs n = fmap (\x -> x <> (T.pack $ show n)) xs <> addSuffixes xs (n+1)
      infiniteSupply = addSuffixes alphabet (0 :: Integer)
      --
      zipped = fmap (\(s, (_, c)) -> (c, s)) $ L.zip infiniteSupply $ M.assocs results
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

getCmpFromMap :: (b -> b -> Bool) -> M.Map a b -> (a,b)
getCmpFromMap cmp map = result
  where aTuple = M.findMax map
        foldFunc = (\o@(kx, cx) k c -> if cmp c cx then (k, c) else o)
        result = M.foldlWithKey foldFunc aTuple map


runElection :: Int -> Int -> Int -> Int -> ElectionM VoteAggregation -> IO ()
runElection numVoters numReps vecSize seed m = do
  print "Starting Election"
  print $ "Voters : " ++ (show numVoters) ++ " Reps: " ++ (show numReps)
  print $ "Vector Size: " ++ show vecSize
  result <- evalStateT (evalStateT m M.empty) (mkStdGen seed)
  print $ "Result Size: " ++  show (length result)
  printResults result
  print "Finished"



--Examples
runFPTP = runElection 1000 20 10 23512122 (firstPastThePost 1000 20 10)
runIRO = runElection 1000 20 10 23512122 (instantRunoff 4 1000 20 10) 
runBorda = runElection 1000 20 10 23512122 (bordaCount 1000 20 10)
runApproval x = runElection 1000 20 10 23512122 (approvalVote 1000 20 10 x)
runScore = runElection 1000 20 10 2351222 (scoreVote 1000 20 10)

main = runFPTP
