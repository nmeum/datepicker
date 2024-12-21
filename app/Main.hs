{-# LANGUAGE PatternSynonyms #-}
module Main where

import qualified Graphics.Vty as V
import qualified Graphics.Vty.Image as I
import qualified Graphics.Vty.Attributes as Attr
import Graphics.Vty.Platform.Unix (mkVty)

import qualified Data.Time.Format as Fmt
import Data.Time.Calendar (Day, DayPeriod, MonthOfYear, dayPeriod, isLeapYear, periodAllDays)
import Data.Time.Calendar.Month (Month, pattern YearMonth)
import Data.Time.Calendar.MonthDay (dayOfYearToMonthAndDay)
import Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import Data.Time.LocalTime (LocalTime, getZonedTime, localDay, zonedTimeToLocalTime)

monthFromTime :: LocalTime -> Month
monthFromTime time =
  let my = fst $ dayOfYearToMonthAndDay (isLeapYear year) yd in
    YearMonth year my
  where
    (year, yd) = toOrdinalDate (localDay time)

drawDay :: Day -> I.Image
drawDay day = I.string attr (show day)
 where
  attr = Attr.defAttr `Attr.withForeColor` Attr.green

drawMonth' :: Month -> I.Image
drawMonth' m = I.string attr (Fmt.formatTime Fmt.defaultTimeLocale "%B" m)
 where
  attr = Attr.defAttr `Attr.withForeColor` Attr.green

drawMonth :: Month -> I.Image
drawMonth m = drawMonth' m I.<-> days
  where
   days :: I.Image
   days = foldl1 (I.<->) $ map drawDay (periodAllDays m)

main :: IO ()
main = do
  vty <- mkVty V.defaultConfig
  localTime <- zonedTimeToLocalTime <$> getZonedTime

  let img = drawMonth (monthFromTime localTime)
      pic = V.picForImage img
  V.update vty pic
  e <- V.nextEvent vty
  V.shutdown vty
  print ("Last event was: " ++ show e)
