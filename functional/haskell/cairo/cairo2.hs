{-# LANGUAGE ScopedTypeVariables #-}
import CairoUtil
import Graphics.Rendering.Cairo
import System.Random
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad
import RandUtils

-- Information
twopi :: Double
twopi = 2.0 * pi

numArcs = 20
arcSize = 0.01

imgSize :: (Int, Int)
imgSize = (1000, 1000)

--Main
main :: IO ()
main = do
  gen <- getStdGen
  let renderer  = evalStateT (bookendDraw) gen
  generalizedCairo "cairo1" imgSize renderer


--Drawing
bookendDraw :: RRender ()
bookendDraw = do
  lift $ save
  let (sx, sy) = tmap fromIntegral imgSize
  lift $ scale sx sy
  lift $ drawRect (1.0, 1.0) -- l$ tmap fromIntegral size
  replicateM_ numArcs $ wrapRand 0.1 0.9 drawArc
  lift $ drawTriangle 0.5 0.5 0.2
  lift $ restore
  

drawRect :: (Double, Double) -> Render ()
drawRect (x, y) = do
  setCLW 0 0 0 1 0.01
  rectangle 0 0 x y
  fill

drawTriangle :: Double -> Double -> Double -> Render ()
drawTriangle x y sz = do
  let halfSz = sz * 0.5
  setCLW 0 1 0 1 0.01
  moveTo (x - halfSz) (y - halfSz)
  lineTo (x + halfSz) (y - halfSz)
  lineTo x (y + halfSz)
  closePath
  stroke

wrapRand :: Double -> Double -> (Double -> Double -> Render ()) -> RRender()
wrapRand low high comp = do
  gen <- splitGen
  let (x,y) = randPair (low, high) gen
  lift $ comp x y

drawArc :: Double -> Double -> Render ()
drawArc x y = do
  setCLW 0 1 0 1 0.01
  -- xc yc r ang1 ang2
  arc x y arcSize 0.0 twopi
  fill

