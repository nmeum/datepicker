module UI (View (..), showView) where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.LocalTime (LocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import System.Exit (exitFailure)
import Util (horizCenter, vertCenter)

class View a where
  draw :: a -> I.Image
  process :: a -> E.Event -> Either (Maybe a) LocalTime
  width :: a -> Int
  height :: a -> Int

showView :: (View a) => a -> (E.Event -> Bool) -> V.Vty -> IO LocalTime
showView v isTermEvent t = showView' v t True
  where
    showView' view vty redraw = do
      let out = V.outputIface vty
      region <- V.displayBounds out

      when redraw $ do
        let (w, h) = (V.regionWidth region, V.regionHeight region)
            img = horizCenter w $ vertCenter h $ draw view
            pic = V.picForImage img
        if width v > w || height v > h
          then V.shutdown vty >> putStrLn ("Terminal is too small: " ++ show w ++ " " ++ show w) >> exitFailure
          else V.update vty pic

      e <- V.nextEvent vty
      if isTermEvent e
        then V.shutdown vty >> exitFailure
        else case process view e of
          Right output -> pure output
          Left mv -> showView' (fromMaybe view mv) vty (isJust mv)
