import System.IO
import Data.Char

main :: IO ()
main = do
  handle <- openFile "test.txt" ReadWriteMode
  contents <- hGetContents handle
  print contents
  print "Test"
  hClose handle
  writeFile "test.txt" "blaaaaaah"
