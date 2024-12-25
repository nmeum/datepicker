{-# LANGUAGE PatternSynonyms #-}

module UI.Month (MonthView, mkMonthView) where

import Data.Bool (bool)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, addMonths, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay), localDay)
import Draw (drawMonth, monthHeight, weekWidth)
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util (addWeeks, makePad)

data MonthView = MonthView
  { months :: [Month],
    curDay :: Cal.Day
  }

instance View MonthView where
  draw = drawView
  process = processEvent

mkMonthView :: LocalTime -> MonthView
mkMonthView time =
  let my = fst $ dayOfYearToMonthAndDay (Cal.isLeapYear year) yd
      mo = YearMonth year my
   in MonthView (map (\n -> addMonths n mo) [0 .. 2]) day
  where
    day = localDay time
    (year, yd) = toOrdinalDate day

drawView :: MonthView -> I.Image
drawView MonthView {curDay = d, months = ms} =
  I.horizCat $ map drawView' ms
  where
    drawView' :: Month -> I.Image
    drawView' m =
      drawMonth m d I.<|> makePad 2 monthHeight I.<-> makePad weekWidth 1

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
hasDay MonthView {months = ms} d =
  any (\m -> Cal.dayPeriod d == m) ms

moveDay :: MonthView -> (Cal.Day -> Cal.Day) -> Maybe MonthView
moveDay mv@MonthView {curDay = d} proc =
  let newDay = proc d
   in bool Nothing (Just mv {curDay = newDay}) (hasDay mv newDay)
