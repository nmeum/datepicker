{-# LANGUAGE PatternSynonyms #-}

module Main where

import Control.Monad (when)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVty)
import UI qualified as UI

isTermEvent :: E.Event -> Bool
isTermEvent (E.EvKey key _) =
  key == E.KEsc || key == E.KChar 'q'
isTermEvent _ = False

inputLoop :: UI.MonthView -> V.Vty -> Bool -> IO ()
inputLoop view vty redraw = do
  when redraw $ do
    let img = UI.drawView view
        pic = V.picForImage img
    V.update vty pic

  e <- V.nextEvent vty
  if isTermEvent e
    then V.shutdown vty
    else case UI.processEvent view e of
      Just newView -> inputLoop newView vty True
      Nothing -> inputLoop view vty False

main :: IO ()
main = do
  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime
  inputLoop (UI.mkMonthView localTime) vty True
