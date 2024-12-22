module Draw (drawHeader, drawMonth, drawWeeks) where

import Data.List (intersperse)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)
import Data.Time.Format qualified as Fmt
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Util (Weeks)

weekWidth :: Int
weekWidth = (2 * 7) + 6 -- +6 for spacing between weeks

drawWeeks :: Cal.Day -> Weeks -> I.Image
drawWeeks curDay w =
  I.vertCat $
    map (\(i, e) -> padWeekImg (i == 0) e) (zip [0 ..] $ map drawWeek w)
  where
    padWeekImg :: Bool -> I.Image -> I.Image
    padWeekImg padLeft i =
      let diff = weekWidth - I.imageWidth i
          comb = if padLeft then I.horizJoin else (flip I.horizJoin)
       in if diff > 0
            then I.charFill Attr.defAttr ' ' diff 1 `comb` i
            else i

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
    drawWeek days = I.horizCat (intersperse (I.string Attr.defAttr " ") $ map drawDay days)

drawMonth :: Month -> I.Image
drawMonth m =
  let img = I.string Attr.defAttr fmt
      diff = fromIntegral $ weekWidth - I.imageWidth img
   in if diff > 0
        then
          let ldiff = floor (diff / 2)
              rdiff = ceiling (diff / 2)
           in I.charFill Attr.defAttr ' ' ldiff 1 I.<|> img I.<|> I.charFill Attr.defAttr ' ' rdiff 1
        else img
  where
    fmt = Fmt.formatTime Fmt.defaultTimeLocale "%B %Y" m

drawHeader :: Fmt.TimeLocale -> I.Image
drawHeader Fmt.TimeLocale {Fmt.wDays = w} =
  let wdays = map snd w
      items = map drawWeekDay $ intersperse " " (map shortenWeekDay wdays)
   in I.horizCat items
  where
    drawWeekDay :: String -> I.Image

    shortenWeekDay :: String -> String
    shortenWeekDay (f : s : _xs) = [f, s]
    shortenWeekDay s = s
    drawWeekDay = I.string Attr.defAttr
