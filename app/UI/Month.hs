module UI.Month (MonthView, mkMonthView) where

import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, addMonths)
import Data.Time.Format qualified as Fmt
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay))
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util

data MonthView = MonthView
  { months :: [Month],
    curDay :: Cal.Day
  }

instance View MonthView where
  draw = drawView
  process = processEvent

currentMonth :: MonthView -> Month
currentMonth MonthView {months = ms, curDay = d} =
  fromJust $ find (\m -> Cal.dayPeriod d == m) ms

hasDay :: MonthView -> Cal.Day -> Bool
hasDay MonthView {months = ms} d =
  any (\m -> Cal.dayPeriod d == m) ms

hasMonth :: MonthView -> Month -> Bool
hasMonth MonthView {months = ms} m =
  any ((==) m) ms

------------------------------------------------------------------------

mkMonthView :: [Month] -> Cal.Day -> MonthView
mkMonthView = MonthView

drawView :: MonthView -> I.Image
drawView MonthView {curDay = d, months = ms} =
  I.vertCat (map I.horizCat $ splitEvery 3 (map drawView' ms))
  where
    drawView' :: Month -> I.Image
    drawView' m =
      let img = drawMonth m d
       in img I.<|> makePad 2 (I.imageHeight img) I.<-> makePad weekWidth 1

-- The return value specifies if the view has changed as a result
-- of processing the event, if so, 'drawView' needs to be invoked.
processEvent :: MonthView -> E.Event -> Either (Maybe MonthView) LocalTime
processEvent view@MonthView {curDay = day} (E.EvKey key _mods) =
  case key of
    E.KEnter -> Right $ LocalTime day (TimeOfDay 0 0 0)
    E.KUp -> Left $ moveCursor view PrevWeek
    E.KDown -> Left $ moveCursor view NextWeek
    E.KRight -> Left $ moveCursor view NextDay
    E.KLeft -> Left $ moveCursor view PrevDay
    _ -> Left Nothing
processEvent view (E.EvResize _ _) = Left $ Just view
processEvent _ _ = Left Nothing

------------------------------------------------------------------------

weekWidth :: Int
weekWidth = (2 * 7) + 6 -- +6 for spacing between weeks

drawDay :: Cal.Day -> Bool -> I.Image
drawDay day curDay =
  let attr = if curDay then high else Attr.defAttr
   in I.string attr $ format "%_2e" day
  where
    high :: Attr.Attr
    high =
      Attr.defAttr
        `Attr.withBackColor` Attr.white
        `Attr.withForeColor` Attr.black

drawWeeks :: Cal.Day -> Weeks -> I.Image
drawWeeks curDay w =
  I.vertCat $ zipWith zipFunc [0 ..] (map drawWeek w)
  where
    zipFunc :: Int -> I.Image -> I.Image
    zipFunc i = horizPad weekWidth (i == 0)

    drawWeek :: [Cal.Day] -> I.Image
    drawWeek days =
      I.horizCat $
        addSep (map (\day -> drawDay day $ day == curDay) days)

-- XXX: Unfortunately, 'MonthYear' does not implement 'FormatTime'.
drawMonthYear :: Month -> I.Image
drawMonthYear m =
  horizCenter weekWidth $
    I.string Attr.defAttr (format "%B %Y" m)

drawMonth :: Month -> Cal.Day -> I.Image
drawMonth m curDay = drawMonthYear m I.<-> drawHeader locale I.<-> weeks
  where
    weeks :: I.Image
    weeks = drawWeeks curDay (monthWeeks m)

drawHeader :: Fmt.TimeLocale -> I.Image
drawHeader Fmt.TimeLocale {Fmt.wDays = w} =
  let wdays = map snd w
      items = map (I.string Attr.defAttr . shortenWeekDay) wdays
   in I.horizCat $ addSep items
  where
    shortenWeekDay :: String -> String
    shortenWeekDay (f : s : _xs) = [f, s]
    shortenWeekDay s = s

------------------------------------------------------------------------

data Direction = NextDay | PrevDay | NextWeek | PrevWeek
  deriving (Eq)

moveByDirection :: Direction -> Cal.Day -> Cal.Day
moveByDirection NextDay = Cal.addDays 1
moveByDirection PrevDay = Cal.addDays (-1)
moveByDirection NextWeek = addWeeks 1
moveByDirection PrevWeek = addWeeks (-1)

-- TODO: Make this customizable to implement the cal(1) -m option.
firstDayOfWeek :: MonthView -> Bool
firstDayOfWeek mv@MonthView {curDay = d} =
  Cal.dayOfWeek d == Cal.Sunday
    || Cal.periodFirstDay (currentMonth mv) == d

-- TODO: Make this customizable to implement the cal(1) -m option.
lastDayOfWeek :: MonthView -> Bool
lastDayOfWeek mv@MonthView {curDay = d} =
  Cal.dayOfWeek d == Cal.Saturday
    || Cal.periodLastDay (currentMonth mv) == d

moveCursor :: MonthView -> Direction -> Maybe MonthView
moveCursor mv@MonthView {curDay = day} dir
  | lastDayOfWeek mv && dir == NextDay = moveMonthwise mv 1 head
  | firstDayOfWeek mv && dir == PrevDay = moveMonthwise mv (-1) last
  -- \| firstWeekOfMonth mv && dir == _
  -- \| lastWeekOfMonth mv && dir == _
  | otherwise =
      let newDay = moveByDirection dir day
       in bool Nothing (Just mv {curDay = newDay}) (hasDay mv newDay)

moveMonthwise :: MonthView -> Integer -> ([Cal.Day] -> Cal.Day) -> Maybe MonthView
moveMonthwise mv@MonthView {curDay = day} inc select =
  let curMonth = currentMonth mv
      newMonth = addMonths inc curMonth
   in if hasMonth mv newMonth
        then
          (\lst -> mv {curDay = select lst})
            <$> nthWeekOfMonth newMonth (weekOfMonth day)
        else Nothing
