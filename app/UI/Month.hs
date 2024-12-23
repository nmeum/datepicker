{-# LANGUAGE PatternSynonyms #-}

module UI.Month (MonthView, mkMonthView) where

import Data.Bool (bool)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay), localDay)
import Draw (drawMonth, monthHeight, weekWidth)
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util (addWeeks, makePad)

data MonthView = MonthView
  { curMonth :: Month,
    curDay :: Cal.Day
  }

instance View MonthView where
  draw = drawView
  process = processEvent

mkMonthView :: LocalTime -> MonthView
mkMonthView time =
  let my = fst $ dayOfYearToMonthAndDay (Cal.isLeapYear year) yd
   in MonthView (YearMonth year my) day
  where
    day = localDay time
    (year, yd) = toOrdinalDate day

drawView :: MonthView -> I.Image
drawView MonthView {curDay = d, curMonth = m} =
  drawMonth m d I.<|> makePad 1 monthHeight I.<-> makePad weekWidth 1

-- The return value specifies if the view has changed as a result
-- of processing the event, if so, 'drawView' needs to be invoked.
processEvent :: MonthView -> E.Event -> Either (Maybe MonthView) LocalTime
processEvent view@MonthView {curDay = day} (E.EvKey key _mods) =
  case key of
    E.KEnter -> Right $ LocalTime day (TimeOfDay 0 0 0)
    E.KUp -> Left $ moveDay view (addWeeks (-1))
    E.KDown -> Left $ moveDay view (addWeeks 1)
    E.KRight -> Left $ moveDay view (Cal.addDays 1)
    E.KLeft -> Left $ moveDay view (Cal.addDays (-1))
    _ -> Left Nothing
processEvent view (E.EvResize _ _) = Left $ Just view
processEvent _ _ = error "not implemented"

------------------------------------------------------------------------

hasDay :: MonthView -> Cal.Day -> Bool
hasDay MonthView {curMonth = m} d = Cal.dayPeriod d == m

moveDay :: MonthView -> (Cal.Day -> Cal.Day) -> Maybe MonthView
moveDay mv@MonthView {curDay = d} proc =
  let newDay = proc d
   in bool Nothing (Just mv {curDay = newDay}) (hasDay mv newDay)
