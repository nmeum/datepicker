module Util (Weeks, monthWeeks, addWeeks) where

import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)

type Weeks = [[Cal.Day]]

monthWeeks :: Month -> Weeks
monthWeeks m = monthWeeks' $ Cal.periodFirstDay m
  where
    weekOfDay :: Cal.Day -> [Cal.Day]
    weekOfDay d = Cal.weekAllDays Cal.Sunday d

    monthWeeks' :: Cal.Day -> Weeks
    monthWeeks' d
      | Cal.dayPeriod d /= m = []
      | otherwise = filter ((==) m . Cal.dayPeriod) (weekOfDay d) : monthWeeks' (addWeeks 1 d)

addWeeks :: Integer -> Cal.Day -> Cal.Day
addWeeks n = Cal.addDays (n * 7)
