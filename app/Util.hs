module Util
  ( Weeks,
    monthWeeks,
    addWeeks,
    horizPad,
    horizCenter,
    vertCenter,
    format,
    addSep,
    locale,
    makePad,
  )
where

import Data.List (intersperse)
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)
import Data.Time.Format qualified as Fmt
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I

type Weeks = [[Cal.Day]]

monthWeeks :: Month -> Weeks
monthWeeks m = monthWeeks' $ Cal.periodFirstDay m
  where
    weekOfDay :: Cal.Day -> [Cal.Day]
    weekOfDay = Cal.weekAllDays Cal.Sunday

    monthWeeks' :: Cal.Day -> Weeks
    monthWeeks' d
      | Cal.dayPeriod d /= m = []
      | otherwise =
          let days = weekOfDay d
              nday = Cal.addDays 1 $ last days
           in filter ((==) m . Cal.dayPeriod) days : monthWeeks' nday

addWeeks :: Integer -> Cal.Day -> Cal.Day
addWeeks n = Cal.addDays (n * 7)

------------------------------------------------------------------------

-- TODO: Make this configurable
locale :: Fmt.TimeLocale
locale = Fmt.defaultTimeLocale

format :: (Fmt.FormatTime t) => String -> t -> String
format = Fmt.formatTime locale

addSep :: [I.Image] -> [I.Image]
addSep = intersperse (I.string Attr.defAttr " ")

makePad :: Int -> Int -> I.Image
makePad = I.charFill Attr.defAttr ' '

horizPad :: Int -> Bool -> I.Image -> I.Image
horizPad w padLeft i =
  let diff = w - I.imageWidth i
      comb = if padLeft then I.horizJoin else flip I.horizJoin
   in if diff > 0
        then makePad diff 1 `comb` i
        else i

horizCenter :: Int -> I.Image -> I.Image
horizCenter w img =
  let diff = fromIntegral (w - I.imageWidth img) :: Double
      ldiff = floor (diff / 2)
      rdiff = ceiling (diff / 2)
   in if diff > 0
        then makePad ldiff 1 I.<|> img I.<|> makePad rdiff 1
        else img

vertCenter :: Int -> I.Image -> I.Image
vertCenter w img =
  let diff = fromIntegral (w - I.imageHeight img) :: Double
      tdiff = floor (diff / 2)
      bdiff = ceiling (diff / 2)
   in if diff > 0
        then makePad 1 tdiff I.<-> img I.<-> makePad 1 bdiff
        else img
