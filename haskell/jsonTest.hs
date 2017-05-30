-- from https://stackoverflow.com/questions/17844223/
-- also see https://github.com/bos/aeson/blob/master/examples/Generic.hs
{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL

data Address = Address
    { house  :: Integer
    , street :: String
    , city   :: String
    , state  :: Maybe String
    , zip    :: Integer
    } deriving (Show, Eq)

data Person = Person
    { name    :: String
    , age     :: Integer
    , address :: Address
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''Person)

aa :: BL.ByteString
aa = "{\"name\": \"some body\", \"age\" : 23, \"address\" : {\"house\" : 285, \"street\" : \"7th Ave.\", \"city\" : \"New York\", \"state\" : \"New York\", \"zip\" : 10001}}"

main = do
  text <- BL.readFile "test.json"
  print (decode text :: Maybe Person)
