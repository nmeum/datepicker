module Util (Weeks, monthWeeks) where

import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month (Month)

type Weeks = [[Cal.Day]]

monthWeeks :: Month -> Weeks
monthWeeks m = monthWeeks' $ Cal.periodFirstDay m
  where
    weekOfDay :: Cal.Day -> [Cal.Day]
    weekOfDay d = Cal.weekAllDays (Cal.dayOfWeek d) d

    monthWeeks' :: Cal.Day -> Weeks
    monthWeeks' d
      | Cal.dayPeriod d /= m = []
      | otherwise = filter ((==) m . Cal.dayPeriod) (weekOfDay d) : monthWeeks' (Cal.addDays 7 d)
