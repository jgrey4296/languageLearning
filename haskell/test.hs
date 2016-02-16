--Typical Typeclasses: Eq,Ord,Show,Read,Bounded,Enum
--Type Declarations
data Point = Point Int Int deriving (Read)
--custom show function, making point part of the show typeclass
instance Show Point where
  show (Point x y) = "X: " ++ (show x) ++ " Y: " ++ (show y)

--can read from a string by: let a = read "Point 4 5" :: Point
data Shape = Circle Point Int | Rect Point Point deriving (Show)

--Type Synonym
type Name = String

--Record:
data Person = Person { firstName :: String
                     , lastName  :: String
                     , age :: Int
                     } deriving (Show, Eq)
--to access: firstName a where a = Person "bob" "bill" 5
fullName :: Person -> String
fullName p = firstName p ++ " " ++ lastName p


--TypeClass Definition:
class MyEq a where
  (#==) :: a -> a -> Bool
  (#/=) :: a -> a -> Bool
  x #== y = not (x #/= y)
  x #/= y = not (x #== y)
--Make a type an instance:
data Light = Red | Yellow | Green
--implement the functions of the typeclass:
instance MyEq Light where
  Red #== Red = True
  Green #== Green = True
  Yellow #== Yellow = True
  _ #== _ = False

-- List comprehension
a = [x*2 | x <- [1,2,3,4], mod x 2 /= 1]


main = do
  putStrLn "Hello"
  x <- getLine
  putStrLn "blah"
  y <- getLine
  let x' = read x :: Int
      y' = read y :: Int
  putStrLn $ show $ x'  +  y'


--- state monad exploration
simp :: String -> State String String
simp inp = do
  z1 <- get --take the state as the value z1
  put (z1 ++ "test")
  return z1 -- combine the value with the state
