module UI.Month (MonthView, mkMonthView) where

import Data.Bool (bool)
import Data.List (find, findIndex)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, addMonths)
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay))
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util

data MonthView = MonthView
  { months :: [Month],
    curDay :: Cal.Day,
    numCols :: Int,
    movType :: Movement,
    weekOrd :: NE.NonEmpty Cal.DayOfWeek
  }

instance View MonthView where
  draw = drawView
  process = processEvent

mkMonthView :: [Month] -> Cal.Day -> Cal.DayOfWeek -> Bool -> MonthView
mkMonthView ms day firstWeekDay logicMove =
  let week = take 7 (enumFrom firstWeekDay)
      move = if logicMove then MLogical else MSpatial
   in MonthView ms day 3 move $ NE.fromList week

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
firstDayOfWeek mv@MonthView {curDay = d, weekOrd = ord} =
  Cal.dayOfWeek d == NE.head ord
    || Cal.periodFirstDay (currentMonth mv) == d

-- TODO: Make this customizable to implement the cal(1) -m option.
lastDayOfWeek :: MonthView -> Bool
lastDayOfWeek mv@MonthView {curDay = d, weekOrd = ord} =
  Cal.dayOfWeek d == NE.last ord
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
drawView MonthView {curDay = d, months = ms, numCols = cols, weekOrd = ord} =
  I.vertCat (map I.horizCat $ splitEvery cols (map drawView' ms))
  where
    drawView' :: Month -> I.Image
    drawView' m =
      let img = drawMonth m ord d
       in img I.<|> makePad 2 (I.imageHeight img) I.<-> makePad weekWidth 1

-- The return value specifies if the view has changed as a result
-- of processing the event, if so, 'drawView' needs to be invoked.
processEvent :: MonthView -> E.Event -> Either (Maybe MonthView) LocalTime
processEvent view@MonthView {curDay = day, movType = mov} (E.EvKey key _) =
  case key of
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

drawWeeks :: Cal.Day -> NE.NonEmpty Week -> I.Image
drawWeeks curDay weeks =
  let w = NE.toList $ NE.map NE.toList weeks
   in I.vertCat $ zipWith zipFunc [0 ..] (map drawWeek w)
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

drawMonth :: Month -> NE.NonEmpty Cal.DayOfWeek -> Cal.Day -> I.Image
drawMonth m ord curDay = drawMonthYear m I.<-> drawHeader ord I.<-> weeks
  where
    weeks :: I.Image
    weeks = drawWeeks curDay (monthWeeks m $ NE.head ord)

drawHeader :: NE.NonEmpty Cal.DayOfWeek -> I.Image
drawHeader ord =
  let wdays = NE.map (format "%a") ord
      items = NE.map (I.string Attr.defAttr . shortenWeekDay) wdays
   in I.horizCat $ addSep (NE.toList items)
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
      moveSpatialHoriz mv 1 NE.head
  | mov == MSpatial && firstDayOfWeek mv && dir == PrevDay =
      moveSpatialHoriz mv (-1) NE.last
  | mov == MSpatial && firstWeekDayOfMonth mv && dir == PrevWeek =
      moveSpatialVert mv (cols * (-1)) NE.reverse
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
  (Month -> Maybe Week) ->
  (Week -> Maybe Cal.Day) ->
  Maybe MonthView
moveSpatial mv inc selectWeek selectDay =
  let curMonth = currentMonth mv
      newMonth = addMonths (fromIntegral inc) curMonth
   in if hasMonth mv newMonth
        then (\d -> mv {curDay = d}) <$> (selectWeek newMonth >>= selectDay)
        else Nothing

moveSpatialVert :: MonthView -> Int -> (Week -> Week) -> Maybe MonthView
moveSpatialVert mv@MonthView {curDay = day} inc proc =
  let dayOfWeek = Cal.dayOfWeek day
   in moveSpatial
        mv
        inc
        (Just . proc . NE.fromList . Cal.periodAllDays)
        (find ((==) dayOfWeek . Cal.dayOfWeek))

moveSpatialHoriz :: MonthView -> Int -> (Week -> Cal.Day) -> Maybe MonthView
moveSpatialHoriz mv@MonthView {curDay = day, weekOrd = ord} inc select =
  moveSpatial mv inc (\m -> monthWeeks m f !? weekOfMonth day) (Just . select)
  where
    f :: Cal.DayOfWeek
    f = NE.head ord

    weekOfMonth :: Cal.Day -> Int
    weekOfMonth d =
      let weeks = monthWeeks (Cal.dayPeriod d) f
       in fromJust $ findIndex (elem d) (NE.toList weeks)

{- ORMOLU_DISABLE -}
-- From https://github.com/ghc/ghc/commit/d53f6f4d98aabd6f5b28fb110db1da0f6db70a06
(!?) :: NE.NonEmpty a -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n

infixl 9 !?
{- ORMOLU_ENABLE -}
