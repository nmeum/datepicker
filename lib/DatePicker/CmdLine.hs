{-# LANGUAGE PatternSynonyms #-}

module DatePicker.CmdLine
  ( Opts (..),
    CmdTime,
    getTime,
    optsPeriod,
    getCmdArgs,
  )
where

import Control.Applicative ((<|>))
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Calendar.OrdinalDate (Day, toOrdinalDate)
import Data.Time.Format qualified as Fmt
import Data.Time.LocalTime (LocalTime (LocalTime))
import DatePicker.Util (locale, periodAllMonths)
import Options.Applicative qualified as OPT

data CmdTime = CmdTime String (Maybe String)

firstDayInMY :: Cal.Year -> Cal.MonthOfYear -> Day
firstDayInMY y my = Cal.periodFirstDay $ YearMonth y my

parseMonth :: String -> IO Month
parseMonth input =
  Fmt.parseTimeM False locale "%B" input
    <|> Fmt.parseTimeM False locale "%b" input
    <|> Fmt.parseTimeM False locale "%m" input

getTime :: LocalTime -> CmdTime -> IO Day
getTime _ (CmdTime month (Just year)) = do
  (YearMonth _ my) <- parseMonth month
  pure (firstDayInMY (read year) my)
getTime (LocalTime cd _) (CmdTime month Nothing) = do
  (YearMonth _ my) <- parseMonth month
  pure (firstDayInMY (fst $ toOrdinalDate cd) my)

------------------------------------------------------------------------

data Duration = OneMonth | ThreeMonths | TwelveMonths | OneYear

data Opts = Opts
  { optNoTime :: Bool,
    optLogical :: Bool,
    optFormat :: String,
    optSelect :: Maybe String,
    optMonday :: Bool,
    optDuration :: Duration,
    optTime :: Maybe CmdTime
  }

durationParser :: OPT.Parser Duration
durationParser =
  OPT.flag
    OneMonth
    OneMonth
    ( OPT.long "one"
        <> OPT.short '1'
        <> OPT.help "Display a single month"
    )
    OPT.<|> OPT.flag'
      ThreeMonths
      ( OPT.long "three"
          <> OPT.short '3'
          <> OPT.help "Display next/previous month for current month"
      )
    OPT.<|> OPT.flag'
      TwelveMonths
      ( OPT.long "twelve"
          <> OPT.short 'Y'
          <> OPT.help "Display the next twelve months"
      )
    OPT.<|> OPT.flag'
      OneYear
      ( OPT.long "year"
          <> OPT.short 'y'
          <> OPT.help "Display the entire year"
      )

timeParser :: OPT.Parser CmdTime
timeParser =
  CmdTime
    <$> OPT.argument OPT.str (OPT.metavar "month")
    <*> OPT.optional
      (OPT.argument OPT.str (OPT.metavar "year"))

optsParser :: OPT.Parser Opts
optsParser =
  Opts
    <$> OPT.switch
      ( OPT.long "date-only"
          <> OPT.short 'd'
          <> OPT.help "Only require date selection, omitting time"
      )
    <*> OPT.switch
      ( OPT.long "logical-move"
          <> OPT.short 'l'
          <> OPT.help "Always move cursor logically by week/date"
      )
    <*> OPT.strOption
      ( OPT.long "format"
          <> OPT.short 'f'
          -- RFC 1123 format as per Go's time package
          <> OPT.value "%a, %d %b %Y %T %Z"
          <> OPT.metavar "FORMAT"
          <> OPT.help "Format in which the date should be output"
      )
    <*> OPT.optional
      ( OPT.strOption $
          OPT.long "select"
            <> OPT.short 's'
            <> OPT.metavar "TIME"
            <> OPT.help "Preselect date/time, in the -f format"
      )
    <*> OPT.switch
      ( OPT.long "monday"
          <> OPT.short 'm'
          <> OPT.help "Treat monday as the first day of the week"
      )
    <*> durationParser
    <*> OPT.optional timeParser

------------------------------------------------------------------------

optsPeriod :: Duration -> Cal.Day -> [Month]
optsPeriod OneMonth day = [Cal.dayPeriod day]
optsPeriod ThreeMonths day =
  let month = Cal.dayPeriod day
   in [addMonths (-1) month, month, addMonths 1 month]
optsPeriod TwelveMonths day =
  let month = Cal.dayPeriod day
   in map (`addMonths` month) [0 .. 11]
optsPeriod OneYear day = periodAllMonths (fst $ toOrdinalDate day)

cmdOpts :: OPT.ParserInfo Opts
cmdOpts =
  OPT.info
    (optsParser OPT.<**> OPT.helper)
    ( OPT.fullDesc
        <> OPT.progDesc "Interactively select a date to be printed to stdout"
        <> OPT.header "datepicker - a utility for interactive date selection"
    )

getCmdArgs :: IO Opts
getCmdArgs = OPT.execParser cmdOpts
