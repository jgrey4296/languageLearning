import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word
import Data.Char (isSpace)

data Greymap = Greymap {
  greyWidth :: Int,
  greyHeight :: Int,
  greyMax :: Int,
  greyData :: L.ByteString
  } deriving (Eq)

data ParseState = ParseState {
  string :: L.ByteString,
  offset :: Int64
  } deriving (Show)

newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
  }

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

--maybe operator:
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

-- Definitions:
identity :: a -> Parse a
parse :: Parse a -> L.ByteString -> Either String a
--parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
--parseP5 :: ParseState -> Either String (a, ParseState)
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
----------


--identity parser:
identity a = Parse (\s -> Right (a, s))

parse parser initState = case runParse parser (ParseState initState 0) of
  Left err -> Left err
  Right (result, _) -> Right result


parseByte :: Parse Word8
parseByte = getState ==> nextState
  where nextState = \initState -> case L.uncons (string initState) of
          Nothing -> bail "no more input"
          Just (byte,remainder) -> putState newState ==> \_ -> identity byte
            where newState = initState { string = remainder, offset = newOffset }
                  newOffset = offset initState + 1


----------
-- Plumbing:
matchHeader prefix str
  | prefix `L8.isPrefixOf` str = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise = Nothing

getNat s = case L8.readInt s of
  Nothing -> Nothing
  Just (num, rest)
    | num <= 0 -> Nothing
    | otherwise -> Just (fromIntegral num, rest)

getBytes n str =
  let count = fromIntegral n
      both@(prefix,_) = L.splitAt count str
  in if L.length prefix < count
     then Nothing
     else Just both

skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)          

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset }


--state manipulation:
getState :: Parse ParseState
getState = Parse (\s-> Right(s,s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $ "byte offset " ++ show (offset s) ++ " : " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
          case runParse firstParser initState of
            Left errMessage -> Left errMessage
            Right (firstResult, newState) -> runParse (secondParser firstResult) newState


instance Functor Parse where
  fmap f parser = parser ==> \result ->
    identity (f result)

-- parseP5 s =
--   matchHeader (L8.pack "P5") s  >>?
--   \s -> skipSpace ((), s) >>?
--         (getNat . snd)     >>?
--         skipSpace         >>?
--         \(width,s) -> getNat s >>?
--                       skipSpace >>?
--                       \(height,s) -> getNat s >>?
--                                      \(maxGrey,s) -> getBytes 1 s >>?
--                                                      (getBytes (width * height) . snd) >>?
--                                                      \(bitmap,s) -> Just (Greymap width height maxGrey bitmap, s)
