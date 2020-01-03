{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
import System.Random
import System.Environment
import Control.Monad.Identity
import Data.Array.ST
import Control.Monad.ST
import Data.STRef
import Text.PrettyPrint


data Street = Street { streetRoad :: Road,
                       streetPavementL :: Pavement,
                       streetPavementR :: Pavement,
                       streetBuildingsL :: [Building],
                       streetBuildingsR :: [Building] } deriving (Show, Eq)


data Road = Road { roadDepth :: Float,
                   roadLength :: Float,
                   roadLanes :: Int } deriving (Show, Eq)

data Pavement = Pavement { pavementDepth :: Float,
                           pavementOffset :: Float } deriving (Show, Eq)

data Building = Building { buildingWidth :: Float,
                           buildingGap :: Float,
                           buildingDepth :: Float
                         } deriving (Show, Eq)


createBasicStreet :: Int -> [Float] -> Street
createBasicStreet lanes streetData = newStreet
  where [sDepth, sLength, pSDepth] = streetData
        theRoad = Road sDepth sLength lanes
        offset = (sDepth + pSDepth) * 0.5
        pL = Pavement pSDepth offset
        pR = Pavement pSDepth $ -offset
        newStreet = Street theRoad pL pR [] []

-- passed in the available size, then a list of building widths
buildingWidthGen :: (Float, [Float], [Float]) -> (Float, [Float], [Float])
buildingWidthGen (remaining, (bWidth:rest), usedWidths)
  | remaining >= bWidth = buildingWidthGen (remaining - bWidth, rest, bWidth:usedWidths)
  | otherwise = (remaining, (bWidth:rest), usedWidths)

buildingWidthGen (_, [], _) = (0.0, [], [])


addOffsets :: Float -> Float -> [Float] -> StdGen -> ([(Float, Float)], StdGen)
addOffsets wMax remain bWidths gen = (tuples, gen''')
  where (shuffled, gen') = shuffle bWidths gen
        (offsets, gen'') = randomiseOffsets wMax remain (length bWidths) gen'
        paddedOffsets = offsets ++ (take (length bWidths - length offsets) $ repeat 0.0)
        (shuffledPads, gen''') = shuffle paddedOffsets gen''
        tuples = zip shuffled shuffledPads

-- Given a max size, a space to fill, and a max number of items to fill it in,
-- create a list of floats that will add up to the space to fill
-- let size = 20 in size == (sum $ fst $ randomiseOffset 2 size 10 $ mkStdGen 2)
randomiseOffsets :: Float -> Float -> Int -> StdGen -> ([Float] , StdGen)
randomiseOffsets oMax remain len gen = (underFlowOffset, gen'')
  where (splitBetween, gen') = randomR (1, len-1) gen
        numOfBuildingsToOffset :: [Int] = take splitBetween $ repeat 0
        paramSubAndStack = subAndStack oMax
        (finalRemain, offsets, gen'') = foldl paramSubAndStack (remain, [], gen') numOfBuildingsToOffset
        underFlowOffset = if finalRemain == 0.0 then offsets else finalRemain:offsets
        

subAndStack :: Float -> (Float, [Float], StdGen) -> Int -> (Float, [Float], StdGen)
subAndStack oMax (remaining, nums, curGen) _
  | remaining > oMax = (remaining', newNums, curGen')
  | otherwise = (0.0, remaining:nums, curGen)
  where (newAmnt :: Float, curGen') = randomR (0, oMax) curGen
        newNums = newAmnt : nums
        remaining' = remaining - newAmnt

--from the haskell wiki
shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen = runST action
  where n = length xs
        toShuffle :: Int -> [a] -> ST s (STArray s Int a)
        toShuffle n' xs' = newListArray (1,n') xs'
        action = do
          g <- newSTRef gen
          let randomRST lohi = do
                (a, s') <- liftM (randomR lohi) (readSTRef g)
                writeSTRef g s'
                return a
          ar <- toShuffle n xs
          xs' <- forM [1..n] $ \i -> do
            j <- randomRST (i, n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
          gen' <- readSTRef g
          return (xs', gen')

--- Combined generator:
makeCompleteStreet :: StdGen -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Street
makeCompleteStreet gen lanes sDepth len pDepth minWidth maxWidth maxOffset minBuildingDepth maxBuildingDepth = completeStreet
  where initialStreet = createBasicStreet lanes [sDepth, len, pDepth]
        (genA, genB) = split gen
        randList = randomRs (minWidth, maxWidth) genA
        (remainL, randList', buildingWidthsL) = buildingWidthGen (len, randList, [])
        (remainR, _, buildingWidthsR) = buildingWidthGen (len, randList', [])
        (offsetTuplesL, genB') = addOffsets maxOffset remainL buildingWidthsL genB
        (offsetTuplesR, genB'') = addOffsets maxOffset remainR buildingWidthsR genB'
        depthList = randomRs (minBuildingDepth, maxBuildingDepth) genB''
        (depthsL, depthList') = splitAt (length offsetTuplesL) depthList
        (depthsR, _) = splitAt (length offsetTuplesR) depthList'
        buildingsL = map (\((w, g), d) -> Building w g d) $ zip offsetTuplesL depthsL
        buildingsR = map (\((w, g), d) -> Building w g d) $ zip offsetTuplesR depthsR
        completeStreet = initialStreet { streetBuildingsL = buildingsL, streetBuildingsR = buildingsR }

extractArgs :: [String] -> [Float]
extractArgs args = finalArgs
  where finalArgs  = if (length args) == 0 then [2, 5, 10, 2, 2, 4, 1, 3, 6]
                    else Prelude.map read args
        
                    
----- Main
filename :: String
filename = "streetSpec.txt"

main :: IO ()
main = do
  args :: [String] <- getArgs
  let [lanes, sDepth, len, pDepth, minW, maxW, maxO, minD, maxD] :: [Float] = extractArgs args
  gen <- newStdGen
  let (genA, _) = split gen
  let street = makeCompleteStreet genA (floor lanes) sDepth len pDepth minW maxW maxO minD maxD
  print $ renderStreet street
  writeFile filename $ render $ renderStreet street

--
indent :: Doc
indent = text "\t"

renderStreet :: Street -> Doc
renderStreet a = output
  where road = text $ show $ streetRoad a
        pavements = map (text . show) [streetPavementL a, streetPavementR a]
        buildings = map (renderBuildings indent) [streetBuildingsL a, streetBuildingsR a]
        bsAndPs = map (\(x,y) -> x $$ y) $ zip pavements buildings
        totalData = indent : road : bsAndPs
        output = text "Street {" <> (foldl1 (\x y -> x $$ indent <> y ) totalData) $$ text "}"

renderBuildings :: Doc -> [Building] -> Doc
renderBuildings ind buildings = iBuildings
  where iBuildings = foldr (\x y -> ind <> indent <> x $$ y) empty $ map (text . show) buildings
    
  

------ Tests

                      
