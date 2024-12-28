module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, addMonths)
import Data.Time.LocalTime (LocalTime (LocalTime), getZonedTime, localDay, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as E
import Graphics.Vty.Platform.Unix (mkVtyWithSettings)
import Graphics.Vty.Platform.Unix.Settings (UnixSettings (settingInputFd, settingOutputFd), defaultSettings)
import Options.Applicative qualified as OPT
import System.Exit (exitFailure)
import System.Posix.IO (OpenMode (ReadWrite), defaultFileFlags, openFd)
import UI qualified
import UI.Month qualified as M
import UI.Time qualified as T
import Util (format, horizCenter, vertCenter)

data Opts = Opts
  { optNoTime :: Bool,
    optFormat :: String,
    optNextPrev :: Bool
  }

optsParser :: OPT.Parser Opts
optsParser =
  Opts
    <$> OPT.switch
      ( OPT.long "date-only"
          <> OPT.short 'd'
          <> OPT.help "Only require date selection, omitting time"
      )
    <*> OPT.option
      OPT.auto
      ( OPT.long "format"
          <> OPT.short 'f'
          <> OPT.value "%c"
          <> OPT.help "Format in which the date should be output"
      )
    <*> OPT.switch
      ( OPT.long "three"
          <> OPT.short '3'
          <> OPT.help "Display next/previous month for current month"
      )

optsPeriod :: Opts -> Cal.Day -> [Month]
optsPeriod opts day =
  if optNextPrev opts
    then [addMonths (-1) month, month, addMonths 1 month]
    else [month]
  where
    month :: Month
    month = Cal.dayPeriod day

cmdOpts :: OPT.ParserInfo Opts
cmdOpts =
  OPT.info
    (optsParser OPT.<**> OPT.helper)
    ( OPT.fullDesc
        <> OPT.progDesc "Print a greeting for TARGET"
        <> OPT.header "hello - a test for optparse-applicative"
    )

------------------------------------------------------------------------

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
  args <- OPT.execParser cmdOpts
  let outFmt = optFormat args

  vty <- unixSettings >>= mkVtyWithSettings V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  let today = localDay localTime
      range = optsPeriod args today
  lt@(LocalTime date _) <- showView (M.mkMonthView range today) vty

  if optNoTime args
    then V.shutdown vty >> putStrLn (format outFmt lt)
    else do
      (LocalTime _ nowTime) <- zonedTimeToLocalTime <$> getZonedTime
      (LocalTime _ time) <- showView (T.mkTimeView nowTime lt) vty

      V.shutdown vty >> putStrLn (format outFmt $ LocalTime date time)
