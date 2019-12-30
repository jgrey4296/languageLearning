data MyTest = TestCtor String 

class AComp a where
  comp :: a -> a -> Bool
  comb :: a -> a -> a

instance AComp MyTest where
  comp (TestCtor a) (TestCtor b) = a == b
  comp _ _ = False

  comb (TestCtor a) (TestCtor b) = TestCtor (a ++ b)

instance Show MyTest where
  show (TestCtor a) = "a testCtor : " ++ a

  
main = do
  putStrLn "First"
  strArg <- getLine
  putStrLn "after"
  let second = testFunc strArg
  putStrLn second



testFunc a
  | a == "blah" = "bloo"
  | a == "awef" = "second"
  | otherwise = "third"
  
           
