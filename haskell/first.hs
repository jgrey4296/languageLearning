import Data.Set as Set
import Control.Monad.State


-- multiples of 3 and 5
muls = Set.fromList $ [x*3 | x <- [1..1000], x*3 < 1000] ++ [x*5 | x <- [1..1000], x*5 < 1000]
sum = Set.foldl (\m v -> m+v) 0 muls


-- even fibonacci numbers

--monad approach to fib:
fib n = flip evalState (0,1) $ do
  forM [0..(n-1)] $ \x -> do
    (a,b) <- get
    put (b, a+b)
    (a,b) <- get
    return a


slice from to xs = take (to - from + 1) (drop from xs)

while pred action accum v1 v2 filterAction
  | (pred current) == False = accum
  | (pred current ) == True  = while pred action newAccum v2 current filterAction
  where current = (action v1 v2)
        newAccum = if filterAction current
                      then current:accum
                   else accum


evenFibs n = Prelude.foldl (\m v-> m+v) 0 evenFibsOverN
  where evenFibsOverN = while (\fibVal -> fibVal < n) (\v1 v2 -> v1+v2) [] 0 1 (\x-> even x)

--palindromes
isPalindrome x = firstHalf == secondHalf
  where asString = show x
        halfLength = div (length asString) 2
        firstHalf = take halfLength asString
        secondHalf = drop halfLength asString

allThreeDigitNumbers = reverse [100..999]

