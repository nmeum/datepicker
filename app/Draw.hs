module Draw (drawHeader, drawMonth, drawWeeks) where

import Data.List (intersperse)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)
import Data.Time.Format qualified as Fmt
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Util (Weeks)

-- TODO: Make this configurable.
startOfWeek :: Cal.DayOfWeek
startOfWeek = Cal.Sunday

padWeekDays :: Int -> I.Image
padWeekDays diff = I.charFill Attr.defAttr ' ' (diff + 2 * diff) 1

drawWeeks :: Cal.Day -> Weeks -> I.Image
drawWeeks curDay w@((fd : _) : _) =
  padWeekDays (Cal.dayOfWeekDiff startOfWeek $ Cal.dayOfWeek fd) I.<-> drawWeeks' w
  where
    drawWeeks' :: Weeks -> I.Image
    drawWeeks' weeks = I.vertCat $ map drawWeek weeks

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
drawWeeks _ _ = error "invalid weeks"

drawMonth :: Month -> I.Image
drawMonth m = I.string Attr.defAttr fmt
  where
    fmt = Fmt.formatTime Fmt.defaultTimeLocale "%B - %Y" m

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
