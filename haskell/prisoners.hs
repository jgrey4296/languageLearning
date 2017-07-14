-- Simple Prisoners Dilemma
import System.Random
import Control.Monad
import Data.Map

type Payoff = (Int, Int)
data Move = Silent | Betray deriving (Show, Eq)
type MatPay = ((Move, Move), Payoff)
data Game = Game { gameMatrix :: [MatPay] } deriving (Show, Eq)
data Strategy = InitStrategy Move | ResponseStrategy { strat :: Map Move Move }


isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

genMoveMatrix :: [a] -> [(a,a)]
genMoveMatrix moves = movePairs
  where movePairs = Prelude.foldl (\x y -> x ++ (zip moves $ cycle [y])) [] moves

makeGame :: [Move] -> [Payoff] -> Game
makeGame moves payoffs = theGame
  where moveMatrix = genMoveMatrix moves
        payoffMatrix = zip moveMatrix payoffs
        theGame = Game payoffMatrix

getPayoffIfMatching :: Move -> Move -> MatPay -> Maybe Payoff
getPayoffIfMatching m1 m2 ((m1b, m2b), p)
  | m1 == m1b && m2 == m2b = Just p
  | otherwise = Nothing

getPayoffFromGame :: Move -> Move -> [MatPay] -> Maybe Payoff
getPayoffFromGame m1 m2 matrix = payoff
  where matching = Prelude.filter isJust $ fmap (getPayoffIfMatching m1 m2) matrix
        payoff = if length matching > 0 then head matching else Nothing

getBestPlayFor :: (Int -> Int -> Bool) -> (Payoff -> Int) -> [MatPay] -> MatPay
getBestPlayFor cmp acc matrix = best
  where getBestPlay mx1@(_, p) mx2@(_, pb) = if acc p `cmp` acc pb then mx1 else mx2
        best = foldl1 getBestPlay matrix


bestResponse cmp move matrix = best
  where best = getBestPlayFor cmp (snd) $ Prelude.filter (\((m, _), _) -> move == m) matrix

bestStart cmp matrix = best
  where best = getBestPlayFor cmp (fst) matrix

---

testGame = makeGame [Silent, Betray] [(1,1), (0,3), (3,0), (2,2)]

