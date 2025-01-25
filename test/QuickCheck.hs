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
    chooseAny,
    chooseInteger,
    testProperty,
  )

monthWeeksAmount :: TestTree
monthWeeksAmount =
  testProperty
    "week amount >= 4"
    (\m -> NE.length (monthWeeks (m :: Month) Cal.Sunday) >= 4)

monthWeeksStartOfMonth :: TestTree
monthWeeksStartOfMonth =
  testProperty "first day of first week is start of month" propFirstDay
  where
    propFirstDay :: Month -> Bool
    propFirstDay m =
      let weeks = monthWeeks m Cal.Sunday
       in (NE.head $ NE.head weeks) == Cal.periodFirstDay m

monthWeeksOrdered :: TestTree
monthWeeksOrdered =
  testProperty "weeks of month are ordered properly" propOrdered
  where
    isSorted :: (Ord a) => [a] -> Bool
    isSorted [] = True
    isSorted [_] = True
    isSorted (x : y : xs)
      | x >= y = False
      | otherwise = isSorted (y : xs)

    propOrdered :: Month -> Bool
    propOrdered m = isSorted $ concat (NE.map NE.toList $ monthWeeks m Cal.Monday)

quickTests :: TestTree
quickTests =
  testGroup
    "QuickCheck Tests"
    [ monthWeeksAmount,
      monthWeeksStartOfMonth,
      monthWeeksOrdered
    ]

instance Arbitrary Month where
  arbitrary = do
    m <- chooseInteger (1, 12)
    y <- chooseAny
    pure $ MkMonth ((y * 12) + (m - 1))
