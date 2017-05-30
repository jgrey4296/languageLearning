{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Root = Root { _name :: String
                 , _point :: Point } deriving (Show, Eq)

data Point = Point { _x :: Int
                   , _y :: Int } deriving (Show, Eq)

--makeLenses ''Root
point :: Lens' Root Point
point = lens _point (\root newPoint -> root { _point = newPoint })

makeLenses ''Point


shiftRootX :: Root -> Root
shiftRootX = over ( point . x) (+ 1)

decRoot :: Int -> Root -> Root
decRoot a = over (point . y) (\x -> x - a)

a = Root "blah" $ Point 2 3

myMod p a = over (point . p) (\x -> x + a)

viewSomething :: Root -> Int
viewSomething = view (point . x)

--setVal p a = over (point . p) (= a)
