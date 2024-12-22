{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.List (intersperse)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.Format qualified as Fmt
import Data.Time.LocalTime (LocalTime, getZonedTime, localDay, zonedTimeToLocalTime)
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Platform.Unix (mkVty)

type Weeks = [[Cal.Day]]

monthFromTime :: LocalTime -> Month
monthFromTime time =
  let my = fst $ dayOfYearToMonthAndDay (Cal.isLeapYear year) yd
   in YearMonth year my
  where
    (year, yd) = toOrdinalDate (localDay time)

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

drawWeeks :: Weeks -> I.Image
drawWeeks w@((fd : _) : _) =
  padWeekDays (Cal.dayOfWeekDiff startOfWeek $ Cal.dayOfWeek fd) I.<-> drawWeeks' w
  where
    drawWeeks' :: Weeks -> I.Image
    drawWeeks' weeks = foldl1 (I.<->) $ map drawWeek weeks

    fmtDay :: Cal.Day -> String
    fmtDay = Fmt.formatTime Fmt.defaultTimeLocale "%_2e"

    drawWeek :: [Cal.Day] -> I.Image
    drawWeek days = foldl1 (I.<|>) (map (I.string Attr.defAttr) (intersperse " " $ map fmtDay days))
drawWeeks _ = error "invalid weeks"

drawMonth' :: Month -> I.Image
drawMonth' m = I.string attr (Fmt.formatTime Fmt.defaultTimeLocale "%B" m)
  where
    attr = Attr.defAttr `Attr.withForeColor` Attr.green

drawMonth :: Month -> I.Image
drawMonth m = drawMonth' m I.<-> drawHeader Fmt.defaultTimeLocale I.<-> weeks
  where
    weeks :: I.Image
    weeks = drawWeeks (monthWeeks m)

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

main :: IO ()
main = do
  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  let out = V.outputIface vty
      img = drawMonth (monthFromTime localTime)
      pic = V.picForImage img
  V.setCursorPos out 1 1
  V.update vty pic
  e <- V.nextEvent vty
  V.shutdown vty
  print ("Last event was: " ++ show e)
