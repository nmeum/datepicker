{-# LANGUAGE CPP #-}

module Main where

import CmdLine
  ( cmdOpts,
    getTime,
    optDuration,
    optFormat,
    optLogical,
    optMonday,
    optNoTime,
    optTime,
    optsPeriod,
  )
import Control.Exception (throwIO)
import Data.Time.Calendar qualified as Cal
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    ZonedTime (ZonedTime),
    getCurrentTimeZone,
    getZonedTime,
    localDay,
    zonedTimeToLocalTime,
  )
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVtyWithSettings)
import Graphics.Vty.Platform.Unix.Settings qualified as VU
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
unixSettings :: IO VU.UnixSettings
unixSettings = do
#if MIN_VERSION_unix(2,8,0)
  ttyFd <- openFd "/dev/tty" ReadWrite defaultFileFlags
#else
  ttyFd <- openFd "/dev/tty" ReadWrite Nothing defaultFileFlags
#endif

  -- Can't build upon defaultSettings here as it flushes standard input and
  -- if standard input is a pipe it may not necessarily be flushable.
  mb <- VU.currentTerminalName
  case mb of
    Nothing -> throwIO VU.MissingTermEnvVar
    Just t -> do
      return $
        VU.UnixSettings
          { VU.settingVmin = 1,
            VU.settingVtime = 100,
            VU.settingInputFd = ttyFd,
            VU.settingOutputFd = ttyFd,
            VU.settingTermName = t
          }

main :: IO ()
main = do
  args <- execParser cmdOpts
  let outFmt = optFormat args

  vty <- unixSettings >>= mkVtyWithSettings V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  today <- case optTime args of
            Nothing -> pure $ localDay localTime
            Just it -> getTime localTime it

  let range = optsPeriod (optDuration args) today
      mview =
        M.mkMonthView
          range
          today
          (if optMonday args then Cal.Monday else Cal.Sunday)
          (optLogical args)
  lt@(LocalTime date _) <- UI.showView mview isTerm vty

  timeZone <- getCurrentTimeZone
  let mkZonedTime local = ZonedTime local timeZone

  if optNoTime args
    then V.shutdown vty >> putStrLn (format outFmt $ mkZonedTime lt)
    else do
      (LocalTime _ nowTime) <- zonedTimeToLocalTime <$> getZonedTime
      (LocalTime _ time) <- UI.showView (T.mkTimeView nowTime lt) isTerm vty

      let res = LocalTime date time
      V.shutdown vty >> putStrLn (format outFmt $ mkZonedTime res)
