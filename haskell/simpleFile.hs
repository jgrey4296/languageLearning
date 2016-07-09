import System.IO
import Data.Char (toUpper)

--Filenames:
inFile = "input.txt"
outFile = "output.txt"

--main:
main :: IO()
main = do
  inh <- openFile inFile ReadMode
  outh <- openFile outFile WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
  do ineof <- hIsEOF inh --check for EOF
     if ineof
        then return () --lifts unit, thus IO ()
        else do inpStr <- hGetLine inh --get a line
                hPutStrLn outh (map toUpper inpStr) --do work
                mainloop inh outh --recurse down
