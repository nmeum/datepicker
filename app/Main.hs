module Main where

import CmdLine (cmdOpts, optDuration, optFormat, optNoTime, optsPeriod)
import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.LocalTime (LocalTime (LocalTime), getZonedTime, localDay, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVtyWithSettings)
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (settingInputFd, settingOutputFd), defaultSettings)
import Options.Applicative (execParser)
import System.Exit (exitFailure)
import System.Posix.IO (OpenMode (ReadWrite), defaultFileFlags, openFd)
import UI qualified
import UI.Month qualified as M
import UI.Time qualified as T
import Util (format, horizCenter, vertCenter)

isTermEvent :: E.Event -> Bool
isTermEvent (E.EvKey key _) =
  key == E.KEsc || key == E.KChar 'q'
isTermEvent _ = False

showView :: (UI.View a) => a -> V.Vty -> IO LocalTime
showView v t = showView' v t True
  where
    showView' view vty redraw = do
      let out = V.outputIface vty
      region <- V.displayBounds out

      when redraw $ do
        let (w, h) = (V.regionWidth region, V.regionHeight region)
            img = horizCenter w $ vertCenter h $ UI.draw view
            pic = V.picForImage img
        V.update vty pic

      e <- V.nextEvent vty
      if isTermEvent e
        then V.shutdown vty >> exitFailure
        else case UI.process view e of
          Right output -> pure output
          Left mv -> showView' (fromMaybe view mv) vty (isJust mv)

-- Make sure we read and write to /dev/tty instead of relying on stdin/stdout.
-- This allows using datepicker within pipes where stdin/stdout is redirected.
unixSettings :: IO UnixSettings
unixSettings = do
  fd <- openFd "/dev/tty" ReadWrite defaultFileFlags

  s <- defaultSettings
  pure s {settingInputFd = fd, settingOutputFd = fd}

main :: IO ()
main = do
  args <- execParser cmdOpts
  let outFmt = optFormat args

  vty <- unixSettings >>= mkVtyWithSettings V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  let today = localDay localTime
      range = optsPeriod (optDuration args) today
  lt@(LocalTime date _) <- showView (M.mkMonthView range today) vty

  if optNoTime args
    then V.shutdown vty >> putStrLn (format outFmt lt)
    else do
      (LocalTime _ nowTime) <- zonedTimeToLocalTime <$> getZonedTime
      (LocalTime _ time) <- showView (T.mkTimeView nowTime lt) vty

      V.shutdown vty >> putStrLn (format outFmt $ LocalTime date time)
