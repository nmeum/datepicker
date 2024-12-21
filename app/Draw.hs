module Draw (drawMonth, weekWidth, monthHeight) where

import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)
import Data.Time.Format qualified as Fmt
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Util (Weeks, addSep, format, horizCenter, horizPad, locale, monthWeeks)

weekWidth :: Int
weekWidth = (2 * 7) + 6 -- +6 for spacing between weeks

monthHeight :: Int
monthHeight = 7

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
      items = map (drawWeekDay . shortenWeekDay) wdays
   in I.horizCat $ addSep items
  where
    drawWeekDay :: String -> I.Image
    drawWeekDay = I.string Attr.defAttr

    shortenWeekDay :: String -> String
    shortenWeekDay (f : s : _xs) = [f, s]
    shortenWeekDay s = s
