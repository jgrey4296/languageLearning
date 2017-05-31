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
  on canvas draw $ myDraw
  on window objectDestroy mainQuit
  window `on` keyPressEvent $ tryEvent $ do
    keyName <- eventKeyName
    case (unpack keyName) of
      "q" -> liftIO $ mainQuit
      "s" -> liftIO $ writePng "test.png" myDraw
      _ -> liftIO $ putStrLn $ "Unrecognised: " ++ unpack keyName

  --Display:
  widgetShowAll window
  mainGUI


myDraw :: Render ()
myDraw = do
  setSourceRGB 1 1 0
  setLineWidth 5
  
  moveTo 120 60
  lineTo 60 110
  lineTo 180 110
  closePath

  stroke


writePng :: String -> Render () -> IO ()
writePng fileName renderCmd = withImageSurface FormatARGB32 windowWidth windowHeight doIt
  where doIt = \result -> do
          renderWith result $ renderCmd
          surfaceWriteToPNG result fileName
  
