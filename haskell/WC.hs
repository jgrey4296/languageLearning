module WC
       (
         myFunction
         ) where


data Customer = Customer {
  id :: Int,
  name :: String,
  address :: String          
  } deriving (Show)



main = interact wordCount
  where wordCount input = show (length (lines input)) ++ (addVal "\n")

addVal :: String -> String
addVal a = a ++ val ++ a ++ other
  where val = "a test"
        other = "blahh"

addVal_other a = let b = "something"
                     c = "other"
                     in a ++ b ++ c ++ "awef"


grab (Just a) = a
grab Nothing = 0

--hides the input parameter, making it essentially parameter-less
quux a = let a = "foo"
             in a ++ "Eek!"

----------
--myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

testMyLength a = if myLength a == length a
                 then True
                 else error "incorrect length"


tMLs = map testMyLength [[1,2,3],[],[2,2,2,2],[2],[11,1,1,1,1,1,1,1,1,1,1]]

toPalindrome [] = []
toPalindrome a = a ++ reverse a

i_isPalindrome [] [] = True
i_isPalindrome [] _ = False
i_isPalindrome _ [] = False
i_isPalindrome (x:xs) (y:ys)
  | x == y      = i_isPalindrome xs ys
  | otherwise   = False

isPalindrome a = i_isPalindrome a (reverse a)


--intersperse:
intersperse sep [] = []
intersperse sep (x:xs) = foldl (\m v-> m ++ sep ++ v) x xs

-- a simple test function
myFunction input = let (x:xs) = lines input in
                       foldl (\m v -> m ++ sep ++ v) x xs
                       where sep = ","
