{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheck (quickTests) where

import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar qualified as Cal
import Data.Time.Calendar.Month
import DatePicker.Util
import Test.Tasty
import Test.Tasty.QuickCheck
  ( Arbitrary,
    arbitrary,
    chooseEnum,
    chooseInteger,
    testProperty,
  )

instance Arbitrary Month where
  arbitrary = do
    m <- chooseInteger (1, 12)
    y <- chooseInteger (0, 2030)
    pure $ MkMonth ((y * 12) + (m - 1))

instance Arbitrary Cal.DayOfWeek where
  arbitrary = chooseEnum (Cal.Monday, Cal.Sunday)

weekList :: Month -> NE.NonEmpty Cal.Day
weekList m =
  NE.fromList $ concatMap NE.toList (monthWeeks m Cal.Monday)

------------------------------------------------------------------------

monthWeeksAmount :: TestTree
monthWeeksAmount =
  testProperty
    "week amount >= 4"
    (\m -> NE.length (monthWeeks (m :: Month) Cal.Sunday) >= 4)

monthWeeksOrdered :: TestTree
monthWeeksOrdered =
  testProperty "weeks of month are ordered properly" propOrdered
  where
    equalsDays :: Month -> [Integer] -> [Cal.Day] -> Bool
    equalsDays m nth days =
      let fstDay = Cal.periodFirstDay m
       in all (\(d, n) -> Cal.addDays n fstDay == d) (zip days nth)

    propOrdered :: Month -> Bool
    propOrdered m = equalsDays m [0 ..] (NE.toList $ weekList m)

monthWeeksBoundaries :: TestTree
monthWeeksBoundaries =
  testProperty "first/last day of month in weeks" propBoundary
  where
    propBoundary :: Month -> Bool
    propBoundary m =
      let w = weekList m
       in NE.head w == Cal.periodFirstDay m
            && NE.last w == Cal.periodLastDay m

monthWeekStartOfWeek :: TestTree
monthWeekStartOfWeek = testProperty "start of week" propWeekStart
  where
    propWeekStart :: Month -> Cal.DayOfWeek -> Bool
    propWeekStart m dw =
      all (\week -> Cal.dayOfWeek (NE.head week) == dw) $
        NE.tail (monthWeeks m dw) -- Doesn't hold for first week

quickTests :: TestTree
quickTests =
  testGroup
    "QuickCheck Tests"
    [ monthWeeksAmount,
      monthWeeksOrdered,
      monthWeeksBoundaries,
      monthWeekStartOfWeek
    ]
