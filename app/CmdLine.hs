module CmdLine
  ( Opts (optNoTime, optLogical, optFormat, optDuration),
    cmdOpts,
    optsPeriod,
  )
where

import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, addMonths)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Options.Applicative qualified as OPT
import Util (periodAllMonths)

data Duration = OneMonth | ThreeMonths | TwelveMonths

data Opts = Opts
  { optNoTime :: Bool,
    optLogical :: Bool,
    optFormat :: String,
    optDuration :: Duration
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
      ( OPT.long "year"
          <> OPT.short 'y'
          <> OPT.help "Display the entire year"
      )

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
    <*> OPT.option
      OPT.str
      ( OPT.long "format"
          <> OPT.short 'f'
          -- RFC 1123 format as per Go's time package
          <> OPT.value "%a, %d %b %Y %T %Z"
          <> OPT.metavar "FORMAT"
          <> OPT.help "Format in which the date should be output"
      )
    <*> durationParser

------------------------------------------------------------------------

optsPeriod :: Duration -> Cal.Day -> [Month]
optsPeriod OneMonth day = [Cal.dayPeriod day]
optsPeriod ThreeMonths day =
  let month = Cal.dayPeriod day
   in [addMonths (-1) month, month, addMonths 1 month]
optsPeriod TwelveMonths day = periodAllMonths (fst $ toOrdinalDate day)

cmdOpts :: OPT.ParserInfo Opts
cmdOpts =
  OPT.info
    (optsParser OPT.<**> OPT.helper)
    ( OPT.fullDesc
        <> OPT.progDesc "Interactively select a date to be printed to stdout"
        <> OPT.header "datepicker - a utility for interactive date selection"
    )
