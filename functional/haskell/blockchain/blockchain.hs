-- TypeSynonymInstances, OverlappingInstances, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses
--{-# LANGUAGE  OverloadedStrings #-}
import Data.Hashable
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Trans.State
import Data.Map

type BlockM a = StateT (Blockchain a) Identity

data Block a = Block { bIndex :: Int
                     , bPrev :: Int
                     , bTime :: Int
                     , bData :: a
                     , bHash :: Int
                     } deriving (Show)

data Blockchain a = Blockchain { bcCurrent :: Block a
                               , bcAllBlocks :: Map Int (Block a)
                               , bcCurrentTime :: Int
                               }

               
generateHash :: Hashable a => Int -> Int -> Int -> a -> Int
generateHash index prev time blockData = hashWithSalt (index + prev + time) blockData

generateInitialBlock :: Block String
generateInitialBlock = Block 0 0 0 "Initial" theHash
  where theHash = generateHash 0 0 0 "Initial"

generateInitialBlockchain :: Blockchain String
generateInitialBlockchain = result
  where initialBlock = generateInitialBlock
        initialMap = fromList [(bHash initialBlock, initialBlock)]
        result = Blockchain initialBlock initialMap 0
  

generateBlock :: Hashable a => a -> BlockM a ()
generateBlock newData = do
  chain <- get
  let prevBlock = bcCurrent chain
      nextIndex = 1 + bIndex prevBlock
      oldHash = bHash prevBlock
      newTime = 1 + bcCurrentTime chain
      newHash = generateHash nextIndex oldHash newTime newData
      newBlock = Block nextIndex oldHash newTime newData newHash
  modify (\x -> x { bcCurrent = newBlock
                  , bcCurrentTime = newTime
                  , bcAllBlocks = insert newHash newBlock $ bcAllBlocks chain })


getBlock :: BlockM String (Block String)
getBlock = do
  x <- gets bcCurrent
  return x

main :: IO ()
main = do
  let result = runIdentity $ evalStateT (generateBlock "blah" >> getBlock) generateInitialBlockchain
  print $ show result
  
