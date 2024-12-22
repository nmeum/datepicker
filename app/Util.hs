module Util (Weeks, monthWeeks, addWeeks, horizPad, horizCenter) where

import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)
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
          filter ((==) m . Cal.dayPeriod) (weekOfDay d)
            : monthWeeks' (addWeeks 1 d)

addWeeks :: Integer -> Cal.Day -> Cal.Day
addWeeks n = Cal.addDays (n * 7)

------------------------------------------------------------------------

horizPad :: Int -> Bool -> I.Image -> I.Image
horizPad w padLeft i =
  let diff = w - I.imageWidth i
      comb = if padLeft then I.horizJoin else flip I.horizJoin
   in if diff > 0
        then I.charFill Attr.defAttr ' ' diff 1 `comb` i
        else i

horizCenter :: Int -> I.Image -> I.Image
horizCenter w img =
  let diff = fromIntegral (w - I.imageWidth img) :: Double
      ldiff = floor (diff / 2)
      rdiff = ceiling (diff / 2)
   in if diff > 0
        then pad ldiff I.<|> img I.<|> pad rdiff
        else img
  where
    pad :: Int -> I.Image
    pad s = I.charFill Attr.defAttr ' ' s 1
