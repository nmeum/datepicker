module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.LocalTime (LocalTime (LocalTime), getZonedTime, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVty)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import UI qualified
import UI.Month qualified as M
import UI.Time qualified as T
import Util (format, horizCenter)

isTermEvent :: E.Event -> Bool
isTermEvent (E.EvKey key _) =
  key == E.KEsc || key == E.KChar 'q'
isTermEvent _ = False

showView :: (UI.View a) => a -> V.Vty -> V.DisplayRegion -> IO (Maybe LocalTime)
showView v t r = showView' v t r True
 where
  showView' view vty region redraw = do
    when redraw $ do
      let (w, _h) = (V.regionWidth region, V.regionHeight region)
          img = horizCenter w $ UI.draw view
          pic = V.picForImage img
      V.update vty pic

    e <- V.nextEvent vty
    if isTermEvent e
      then pure Nothing
      else case UI.process view e of
        Right output -> pure $ Just output
        Left mv -> showView' (fromMaybe view mv) vty region (isJust mv)

main :: IO ()
main = do
  args <- getArgs
  when (length args > 1) $ do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName ++ " [FORMAT]"
    hPutStrLn stderr ""
    hPutStrLn stderr "Format documentation: https://hackage.haskell.org/package/time/docs/Data-Time-Format.html#v:formatTime"
    exitFailure

  let outFmt = case args of
        [] -> "%c"
        x : _ -> x

  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  let out = V.outputIface vty
  region <- V.displayBounds out

  maybeDate <- showView (M.mkMonthView localTime) vty region
  case maybeDate of
    Nothing -> V.shutdown vty >> exitFailure
    (Just l@(LocalTime date _)) -> do
      maybeTime <- showView T.mkTimeView vty region
      V.shutdown vty

      putStrLn $ case maybeTime of
        (Just (LocalTime _ time)) ->
          format outFmt (LocalTime date time)
        _ -> format outFmt l
