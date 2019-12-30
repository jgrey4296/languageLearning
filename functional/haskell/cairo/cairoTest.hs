-- originally from http://www.muitovar.com/gtk2hs/app1.html but that is outdated
import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Data.Text

windowWidth = 800
windowHeight = 800

main :: IO ()
main = do
  --Setup
  initGUI
  window <- windowNew
  set window [ windowTitle := "Hello Cairo"
             , windowDefaultWidth := windowWidth
             , windowDefaultHeight := windowHeight
             , containerBorderWidth := 30 ]
  frame <- frameNew
  canvas <- drawingAreaNew

  --Adding
  containerAdd window frame
  containerAdd frame canvas
  widgetModifyBg canvas StateNormal (Color 65535 65535 65535)

  -- Listeners
  on canvas draw $ mainDraw
  on window objectDestroy mainQuit
  window `on` keyPressEvent $ tryEvent $ do
    keyName <- eventKeyName
    case (unpack keyName) of
      "q" -> liftIO $ mainQuit
      "s" -> liftIO $ writePng "test.png" mainDraw
      _ -> liftIO $ putStrLn $ "Unrecognised: " ++ unpack keyName

  --Display:
  widgetShowAll window
  mainGUI

mainDraw :: Render () 
mainDraw = myDraw >> myDraw2


myDraw :: Render ()
myDraw = do
  setSourceRGB 1 1 0
  setLineWidth 5
  moveTo 120 60
  lineTo 60 110
  lineTo 180 110
  closePath
  stroke

myDraw2 :: Render ()
myDraw2 = do
  setSourceRGB 0 1 0
  setLineWidth 10
  moveTo 600 500
  lineTo 400 400
  lineTo 200 400
  lineTo 500 300
  closePath
  fill


writePng :: String -> Render () -> IO ()
writePng fileName renderCmd = withImageSurface FormatARGB32 windowWidth windowHeight doIt
  where doIt = \result -> do
          renderWith result $ renderCmd
          surfaceWriteToPNG result fileName
  
