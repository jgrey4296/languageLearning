module CairoUtil where

import System.Random
import System.IO
import Control.Monad.Trans.Identity
import Control.Monad.Trans
import Control.Monad.Trans.State
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Text

type RRender = StateT StdGen Render

--map over a tuple
tmap :: (a -> b) -> (a, a) -> (b, b)
tmap f (a, b) = (f a, f b)


-- Setup colour and line width
setCLW :: Double -> Double -> Double -> Double -> Double -> Render ()
setCLW r g b a lw = do
  setSourceRGBA r g b a
  setLineWidth lw

writePng :: String -> (Int, Int) -> Render () -> IO ()
writePng fileName (x,y) renderCmd = output
  where doIt = \result -> do
          renderWith result $ renderCmd
          surfaceWriteToPNG result fileName
        output = withImageSurface FormatARGB32 x y doIt

generalizedCairo :: String -> (Int, Int) -> Render () -> IO ()
generalizedCairo name (x,y) drawRoutine = do
  initGUI
  window <- windowNew
  set window [ windowTitle := name
             , windowDefaultWidth := x
             , windowDefaultHeight := y
             , containerBorderWidth := 30 ]
  frame <- frameNew
  canvas <- drawingAreaNew
  -- Add ui components:
  containerAdd window frame
  containerAdd frame canvas
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

  --Listeners:
  on canvas draw $ drawRoutine
  on window objectDestroy mainQuit
  window `on` keyPressEvent $ tryEvent $ do
    keyName <- eventKeyName
    case (unpack keyName) of
      "q" -> liftIO $ mainQuit
      "s" -> liftIO $ writePng (name ++ ".png") (x,y) drawRoutine
      _ -> liftIO $ putStrLn $ "Unrecognised: " ++ unpack keyName

  --Display:
  widgetShowAll window
  mainGUI
  
