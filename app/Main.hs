{-# LANGUAGE CPP #-}

module Main where

import Control.Exception (throwIO)
import Data.Time.Calendar qualified as Cal
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    ZonedTime (ZonedTime),
    getCurrentTimeZone,
    getZonedTime,
    localDay,
    localTimeOfDay,
    zonedTimeToLocalTime,
  )
import DatePicker.CmdLine
  ( getCmdArgs,
    getTime,
    optDuration,
    optFormat,
    optLogical,
    optMonday,
    optNoTime,
    optSelect,
    optTime,
    optsPeriod,
  )
import DatePicker.UI qualified as UI
import DatePicker.UI.Month qualified as M
import DatePicker.UI.Time qualified as T
import DatePicker.Util (format, parseTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVtyWithSettings)
import Graphics.Vty.Platform.Unix.Settings qualified as VU
import System.Posix.IO (OpenMode (ReadWrite), defaultFileFlags, openFd)

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
  args <- getCmdArgs
  let dateFmt = optFormat args

  localTime <- zonedTimeToLocalTime <$> getZonedTime
  baseDay <- case optTime args of
    Nothing -> pure $ localDay localTime
    Just it -> getTime localTime it
  LocalTime selDay selTime <- case optSelect args of
    Nothing -> pure (LocalTime baseDay $ localTimeOfDay localTime)
    Just sd -> parseTime False dateFmt sd

  let range = optsPeriod (optDuration args) baseDay
      mview =
        M.mkMonthView
          range
          selDay
          (if optMonday args then Cal.Monday else Cal.Sunday)
          (optLogical args)
  view <- case mview of
    Nothing -> fail $ "specified date (" ++ show selDay ++ ") is not in displayed range"
    Just x -> pure x

  vty <- unixSettings >>= mkVtyWithSettings V.defaultConfig
  lt@(LocalTime date _) <- UI.showView view isTerm vty

  timeZone <- getCurrentTimeZone
  let mkZonedTime local = ZonedTime local timeZone

  if optNoTime args
    then V.shutdown vty >> putStrLn (format dateFmt $ mkZonedTime lt)
    else do
      (LocalTime _ time) <- UI.showView (T.mkTimeView selTime lt) isTerm vty

      let res = LocalTime date time
      V.shutdown vty >> putStrLn (format dateFmt $ mkZonedTime res)
