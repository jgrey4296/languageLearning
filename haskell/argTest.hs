import System.Environment

main = do
  [x, y, z] <- getArgs
  print x
  print $ "blah: " ++ y
  print z
