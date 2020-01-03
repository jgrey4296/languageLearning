-- file: ch07/tempfile.hs
import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catchIOError)
import Control.Exception(finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
  do 
    putStrLn $ "I have a temporary file at " ++ tempname
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos
    -- Now, write some data to the temporary file
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing "
      ++  show (length tempdata) ++ " bytes: " ++ tempdata
    hPutStrLn temph tempdata
    -- Get our new position. This doesn't actually modify pos
    -- in memory, but makes the name "pos" correspond to a different
    -- value for the remainder of the "do" block.
    pos <- hTell temph
    putStrLn $ "After writing, my new position is " ++ show pos
    putStrLn $ "The file content is: "
    hSeek temph AbsoluteSeek 0
    c <- hGetContents temph
    putStrLn c
    putStrLn $ "Which could be expressed as this Haskell literal:"
    print c

    
withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
  do
    tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
    (tempfile, temph) <- openTempFile tempdir pattern
    finally (func tempfile temph) (do { hClose temph; removeFile tempfile; })
