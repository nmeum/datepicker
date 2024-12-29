module Main where

import CmdLine (cmdOpts, optDuration, optFormat, optNoTime, optsPeriod)
import Data.Time.LocalTime (LocalTime (LocalTime), getZonedTime, localDay, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVtyWithSettings)
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (settingInputFd, settingOutputFd), defaultSettings)
import Options.Applicative (execParser)
import System.Posix.IO (OpenMode (ReadWrite), defaultFileFlags, openFd)
import UI qualified
import UI.Month qualified as M
import UI.Time qualified as T
import Util (format)

isTerm :: E.Event -> Bool
isTerm (E.EvKey key _) =
  key == E.KEsc || key == E.KChar 'q'
isTerm _ = False

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
  lt@(LocalTime date _) <- UI.showView (M.mkMonthView range today) isTerm vty

  if optNoTime args
    then V.shutdown vty >> putStrLn (format outFmt lt)
    else do
      (LocalTime _ nowTime) <- zonedTimeToLocalTime <$> getZonedTime
      (LocalTime _ time) <- UI.showView (T.mkTimeView nowTime lt) isTerm vty

      V.shutdown vty >> putStrLn (format outFmt $ LocalTime date time)
