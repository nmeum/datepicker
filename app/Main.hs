module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.LocalTime (getZonedTime, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVty)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UI qualified

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
    then V.shutdown vty >> exitFailure
    else case UI.processEvent view e of
      Right output -> V.shutdown vty >> putStrLn output
      Left mv -> inputLoop (fromMaybe view mv) vty (isJust mv)

main :: IO ()
main = do
  args <- getArgs
  when (length args > 1) $ do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName ++ " [FORMAT]"
    hPutStrLn stderr ""
    hPutStrLn stderr "Format documentation: https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime"
    exitFailure

  let fmt = case args of
        [] -> "%F"
        x : _ -> x

  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime
  inputLoop (UI.mkMonthView localTime fmt) vty True
