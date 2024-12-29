module UI.Month (MonthView, mkMonthView) where

import Data.Bool (bool)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay))
import Draw (drawMonth, monthHeight, weekWidth)
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util (addWeeks, makePad, splitEvery)

data MonthView = MonthView
  { months :: [Month],
    curDay :: Cal.Day
  }

instance View MonthView where
  draw = drawView
  process = processEvent
  width (MonthView {months = m}) = (weekWidth + 2) * (min 3 $ length m)
  height (MonthView {months = m}) = 8 * (length $ splitEvery 3 m)

mkMonthView :: [Month] -> Cal.Day -> MonthView
mkMonthView = MonthView

drawView :: MonthView -> I.Image
drawView MonthView {curDay = d, months = ms} =
  I.vertCat (map I.horizCat $ splitEvery 3 (map drawView' ms))
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
