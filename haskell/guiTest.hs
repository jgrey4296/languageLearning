-- from https://www.stackbuilders.com/tutorials/haskell/gui-application/
-- needs gtk !! 3 !!
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

main :: IO ()
main = do
  cliArgs <-  initGUI
  window <- windowNew
  set window [ windowTitle := "Test GUI"
             , windowResizable  := False
             , windowDefaultWidth := 230
             , windowDefaultHeight := 250 ]

  grid <- createGrid
  display <- createDisplay
  
  let attach x y w h item = gridAttach grid item x y w h
  attach 0 0 5 1 display
  mkBtn "MC" >>= \x -> attach 0 1 1 1 x


  containerAdd window grid
  widgetShowAll window
  mainGUI


createDisplay = do
  display <- entryNew
  set display [ entryEditable := False
              , entryXalign := 1
              , entryText := "0" ]
  return display

createGrid :: IO Grid
createGrid = do
  grid <- gridNew
  gridSetRowHomogeneous grid True
  return grid



mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn
