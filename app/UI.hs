{-# LANGUAGE PatternSynonyms #-}

module UI (MonthView, mkMonthView, drawView, processKey) where

import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format qualified as Fmt
import Data.Time.LocalTime (LocalTime, localDay)
import Draw (drawHeader, drawMonth, drawWeeks)
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events (Event)
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

-- Need to invoke 'drawView' after to obtain an updated image.
processKey :: MonthView -> Event -> MonthView
processKey _ _ = error "not implemented"
