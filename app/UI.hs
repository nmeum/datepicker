{-# LANGUAGE PatternSynonyms #-}

module UI (MonthView, mkMonthView, drawView, processKey) where

import Data.List (intersperse)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format qualified as Fmt
import Data.Time.LocalTime (LocalTime, localDay)
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events (Event)

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
drawView MonthView{curDay=d, curMonth=m} =
    drawMonth' m I.<-> drawHeader Fmt.defaultTimeLocale I.<-> weeks
  where
    weeks :: I.Image
    weeks = drawWeeks d (monthWeeks m)

-- Need to invoke 'drawView' after to obtain an updated image.
processKey :: Event -> MonthView
processKey = error "not implemented"

------------------------------------------------------------------------

type Weeks = [[Cal.Day]]

-- TODO: Make this configurable.
startOfWeek :: Cal.DayOfWeek
startOfWeek = Cal.Sunday

monthWeeks :: Month -> Weeks
monthWeeks m = monthWeeks' $ Cal.periodFirstDay m
  where
    weekOfDay :: Cal.Day -> [Cal.Day]
    weekOfDay d = Cal.weekAllDays (Cal.dayOfWeek d) d

    monthWeeks' :: Cal.Day -> Weeks
    monthWeeks' d
      | Cal.dayPeriod d /= m = []
      | otherwise = filter ((==) m . Cal.dayPeriod) (weekOfDay d) : monthWeeks' (Cal.addDays 7 d)

padWeekDays :: Int -> I.Image
padWeekDays diff = I.charFill Attr.defAttr ' ' (diff + 2 * diff) 1

drawWeeks :: Cal.Day -> Weeks -> I.Image
drawWeeks curDay w@((fd : _) : _) =
  padWeekDays (Cal.dayOfWeekDiff startOfWeek $ Cal.dayOfWeek fd) I.<-> drawWeeks' w
  where
    drawWeeks' :: Weeks -> I.Image
    drawWeeks' weeks = foldl1 (I.<->) $ map drawWeek weeks

    fmtDay :: Cal.Day -> String
    fmtDay = Fmt.formatTime Fmt.defaultTimeLocale "%_2e"

    drawDay :: Cal.Day -> I.Image
    drawDay day = I.string attr $ fmtDay day
      where
        attr =
          if day == curDay
            then Attr.defAttr `Attr.withBackColor` Attr.white `Attr.withForeColor` Attr.black
            else Attr.defAttr

    drawWeek :: [Cal.Day] -> I.Image
    drawWeek days = foldl1 (I.<|>) (intersperse (I.string Attr.defAttr " ") $ map drawDay days)
drawWeeks _ _ = error "invalid weeks"

drawMonth' :: Month -> I.Image
drawMonth' m = I.string Attr.defAttr fmt
  where
    fmt = Fmt.formatTime Fmt.defaultTimeLocale "%B - %Y" m

drawHeader :: Fmt.TimeLocale -> I.Image
drawHeader Fmt.TimeLocale {Fmt.wDays = w} =
  let wdays = map snd w
      items = map drawWeekDay $ intersperse " " (map shortenWeekDay wdays)
   in foldl1 (I.<|>) items
  where
    drawWeekDay :: String -> I.Image
    drawWeekDay wday = I.string Attr.defAttr wday

    shortenWeekDay :: String -> String
    shortenWeekDay (f : s : _xs) = f : s : []
    shortenWeekDay s = s
