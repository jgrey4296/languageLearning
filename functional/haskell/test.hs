

toOutput :: String -> String
toOutput x = "Result: " ++ x


main = do
  x <- getLine;
  putStrLn "Next:"
  y <- getLine;
  putStrLn $ toOutput y ++ toOutput x
