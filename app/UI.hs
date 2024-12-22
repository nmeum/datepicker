{-# LANGUAGE PatternSynonyms #-}

module UI (MonthView, mkMonthView, drawView, processEvent) where

import Data.Bool (bool)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format qualified as Fmt
import Data.Time.LocalTime (LocalTime, localDay)
import Draw (drawHeader, drawMonth, drawWeeks)
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import Util (monthWeeks)

data MonthView = MonthView
  { curMonth :: Month,
    curDay :: Cal.Day
  }

mkMonthView :: LocalTime -> MonthView
mkMonthView time =
  let my = fst $ dayOfYearToMonthAndDay (Cal.isLeapYear year) yd
   in MonthView (YearMonth year my) day
  where
    day = (localDay time)
    (year, yd) = toOrdinalDate day

drawView :: MonthView -> I.Image
drawView MonthView {curDay = d, curMonth = m} =
  drawMonth m I.<-> drawHeader Fmt.defaultTimeLocale I.<-> weeks
  where
    weeks :: I.Image
    weeks = drawWeeks d (monthWeeks m)

-- The return value specifies if the view has changed as a result
-- of processing the event, if so, 'drawView' needs to be invoked.
processEvent :: MonthView -> E.Event -> Maybe MonthView
processEvent view (E.EvKey key _mods) =
  case key of
    E.KEsc -> Nothing
    E.KRight -> incDay view
    _ -> Just view
processEvent _ _ = error "not implemented"

------------------------------------------------------------------------

hasDay :: MonthView -> Cal.Day -> Bool
hasDay MonthView{ curMonth = m } d = Cal.dayPeriod d == m

incDay :: MonthView -> Maybe MonthView
incDay mv@MonthView {curDay = d} =
  let nextDay = Cal.addDays 1 d in
    bool Nothing (Just mv { curDay = nextDay }) (hasDay mv nextDay)
