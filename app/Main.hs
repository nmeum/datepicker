{-# LANGUAGE PatternSynonyms #-}

module Main where

import UI qualified as UI
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Platform.Unix (mkVty)

main :: IO ()
main = do
  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  -- A view displayin a single month (the current one).
  let view = UI.mkMonthView localTime

  let out = V.outputIface vty
      img = UI.drawView view
      pic = V.picForImage img
  V.setCursorPos out 1 1
  V.update vty pic
  e <- V.nextEvent vty
  V.shutdown vty
  print ("Last event was: " ++ show e)
