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
    curDay :: Cal.Day,
    numCols :: Int
  }

instance View MonthView where
  draw = drawView
  process = processEvent

mkMonthView :: [Month] -> Cal.Day -> MonthView
mkMonthView ms day = MonthView ms day 3

currentMonth :: MonthView -> Month
currentMonth MonthView {months = ms, curDay = d} =
  fromJust $ find (\m -> Cal.dayPeriod d == m) ms

hasDay :: MonthView -> Cal.Day -> Bool
hasDay MonthView {months = ms} d =
  any (\m -> Cal.dayPeriod d == m) ms

hasMonth :: MonthView -> Month -> Bool
hasMonth MonthView {months = ms} m =
  m `elem` ms

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

firstWeekDayOfMonth :: MonthView -> Bool
firstWeekDayOfMonth mv@MonthView {curDay = day} =
  firstWeekDay (currentMonth mv) (Cal.dayOfWeek day) == day
  where
    firstWeekDay :: Month -> Cal.DayOfWeek -> Cal.Day
    firstWeekDay m dw = Cal.firstDayOfWeekOnAfter dw (Cal.periodFirstDay m)

lastWeekDayOfMonth :: MonthView -> Bool
lastWeekDayOfMonth mv@MonthView {curDay = day} =
  Just day == lastWeekDay (currentMonth mv) (Cal.dayOfWeek day)
  where
    lastWeekDay :: Month -> Cal.DayOfWeek -> Maybe Cal.Day
    lastWeekDay m dw = find ((==) dw . Cal.dayOfWeek) $ reverse (Cal.periodAllDays m)

------------------------------------------------------------------------

drawView :: MonthView -> I.Image
drawView MonthView {curDay = d, months = ms, numCols = cols} =
  I.vertCat (map I.horizCat $ splitEvery cols (map drawView' ms))
  where
    drawView' :: Month -> I.Image
    drawView' m =
      let img = drawMonth m d
       in img I.<|> makePad 2 (I.imageHeight img) I.<-> makePad weekWidth 1

-- The return value specifies if the view has changed as a result
-- of processing the event, if so, 'drawView' needs to be invoked.
processEvent :: MonthView -> E.Event -> Either (Maybe MonthView) LocalTime
processEvent view@MonthView {curDay = day} (E.EvKey key mods) =
  let mov = if E.MShift `elem` mods then MLogical else MSpatial
   in case key of
        E.KEnter -> Right $ LocalTime day (TimeOfDay 0 0 0)
        E.KUp -> Left $ moveCursor view mov PrevWeek
        E.KDown -> Left $ moveCursor view mov NextWeek
        E.KRight -> Left $ moveCursor view mov NextDay
        E.KLeft -> Left $ moveCursor view mov PrevDay
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
  deriving (Eq, Show)

data Movement = MLogical | MSpatial
  deriving (Eq, Show)

moveCursor :: MonthView -> Movement -> Direction -> Maybe MonthView
moveCursor mv@MonthView {curDay = day, numCols = cols} mov dir
  | mov == MSpatial && lastDayOfWeek mv && dir == NextDay =
      moveSpatialHoriz mv 1 head
  | mov == MSpatial && firstDayOfWeek mv && dir == PrevDay =
      moveSpatialHoriz mv (-1) last
  | mov == MSpatial && firstWeekDayOfMonth mv && dir == PrevWeek =
      moveSpatialVert mv (cols * (-1)) reverse
  | mov == MSpatial && lastWeekDayOfMonth mv && dir == NextWeek =
      moveSpatialVert mv cols id
  | otherwise =
      let newDay = moveLogical dir day
       in bool Nothing (Just mv {curDay = newDay}) (hasDay mv newDay)

moveLogical :: Direction -> Cal.Day -> Cal.Day
moveLogical NextDay = Cal.addDays 1
moveLogical PrevDay = Cal.addDays (-1)
moveLogical NextWeek = addWeeks 1
moveLogical PrevWeek = addWeeks (-1)

moveSpatial ::
  MonthView ->
  Int ->
  (Month -> Maybe [Cal.Day]) ->
  ([Cal.Day] -> Maybe Cal.Day) ->
  Maybe MonthView
moveSpatial mv inc selectWeek selectDay =
  let curMonth = currentMonth mv
      newMonth = addMonths (fromIntegral inc) curMonth
   in if hasMonth mv newMonth
        then (\d -> mv {curDay = d}) <$> (selectWeek newMonth >>= selectDay)
        else Nothing

moveSpatialVert :: MonthView -> Int -> ([Cal.Day] -> [Cal.Day]) -> Maybe MonthView
moveSpatialVert mv@MonthView {curDay = day} inc proc =
  let dayOfWeek = Cal.dayOfWeek day
   in moveSpatial
        mv
        inc
        (Just . proc . Cal.periodAllDays)
        (find ((==) dayOfWeek . Cal.dayOfWeek))

moveSpatialHoriz :: MonthView -> Int -> ([Cal.Day] -> Cal.Day) -> Maybe MonthView
moveSpatialHoriz mv@MonthView {curDay = day} inc select =
  moveSpatial mv inc (\m -> nthWeekOfMonth m (weekOfMonth day)) (Just . select)
