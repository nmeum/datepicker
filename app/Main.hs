{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix (mkVty)
import UI qualified as UI

inputLoop :: UI.MonthView -> V.Vty -> IO ()
inputLoop view vty = do
  let img = UI.drawView view
      pic = V.picForImage img
  V.update vty pic
  e <- V.nextEvent vty

  case UI.processKey view e of
    Just newView -> inputLoop newView vty
    _ -> V.shutdown vty

main :: IO ()
main = do
  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime
  inputLoop (UI.mkMonthView localTime) vty
