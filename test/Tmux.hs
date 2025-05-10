{-# LANGUAGE OverloadedStrings #-}

module Tmux (tmuxTests) where

import Control.Monad (replicateM_)
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.Tmux
import Util

selectDateSpatially :: TestCase sharedEnv
selectDateSpatially =
  withTmuxSession' "select date using spatial movement" $ \_ -> do
    startApplication ["--date-only"] "dec 2024"

    -- selection: 2024-12-01
    sendKeys_ "Up" Unconditional
    sendKeys_ "Left" Unconditional
    -- selection: 2024-12-01
    sendKeys_ "Down" Unconditional
    -- selection: 2024-12-08
    sendKeys_ "Right" Unconditional
    -- selection: 2024-12-09
    sendKeys_ "Right" Unconditional
    -- selection: 2024-12-10
    sendKeys_ "Down" Unconditional
    -- selection: 2024-12-17
    sendKeys_ "Down" Unconditional
    -- selection: 2024-12-24
    sendKeys_ "Left" Unconditional
    -- selection: 2024-12-23

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Mon, 23 Dec 2024 00:00:00 UTC"

selectDateLogically :: TestCase sharedEnv
selectDateLogically =
  withTmuxSession' "select date using logical movement" $ \_ -> do
    startApplication ["--date-only", "--logical-move"] "02 2020"

    -- 2020-02-01 is a Saturday, so moving right, in logical mode, will
    -- directly go to the next week. Hence moving up after should do nothing.
    sendKeys_ "Right" Unconditional
    -- selection: 2020-02-02
    sendKeys_ "Up" Unconditional
    -- selection: 2020-02-02
    sendKeys_ "Down" Unconditional
    -- selection: 2020-02-09
    sendKeys_ "Left" Unconditional
    -- selection: 2020-02-08

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Sat, 08 Feb 2020 00:00:00 UTC"

selectDateWithCustomFormat :: TestCase sharedEnv
selectDateWithCustomFormat =
  withTmuxSession' "custom date format" $ \_ -> do
    startApplication ["--date-only", "--format", "'%0Y%m%d %Z'"] "July 1998"

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "19980701 UTC"

selectDateMondayWeekstart :: TestCase sharedEnv
selectDateMondayWeekstart =
  withTmuxSession' "--monday option" $ \_ -> do
    startApplication ["--date-only", "--monday"] "Jan 2025"

    sendKeys_ "Down" Unconditional
    -- selection: 2025-08-01
    sendKeys_ "Left" Unconditional
    -- selection: 2025-07-01
    sendKeys_ "Left" Unconditional
    sendKeys_ "Left" Unconditional
    sendKeys_ "Left" Unconditional
    sendKeys_ "Left" Unconditional
    -- selection: 2025-06-01

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Mon, 06 Jan 2025 00:00:00 UTC"

selectDateTwelveMonths :: TestCase sharedEnv
selectDateTwelveMonths =
  withTmuxSession' "--twelve option" $ \_ -> do
    startApplication ["--date-only", "--twelve"] "sep 2025"

    replicateM_ 17 (sendKeys_ "Down" Unconditional)
    -- selection: 2025-09-15
    replicateM_ 15 (sendKeys_ "Right" Unconditional)
    -- selection: 2025-08-11

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Tue, 11 Aug 2026 00:00:00 UTC"

selectTime :: TestCase sharedEnv
selectTime =
  withTmuxSession' "select date and time" $ \_ -> do
    startApplication [] "03 1900"

    sendKeys_ "Enter" Unconditional
    sendKeys_ "2342" Unconditional
    sendKeys_ "Enter" Unconditional

    captureDate >>= assertDate "Thu, 01 Mar 1900 23:42:00 UTC"

selectTimeAndOverflow :: TestCase sharedEnv
selectTimeAndOverflow =
  withTmuxSession' "select time, delete, and re-enter" $ \_ -> do
    startApplication [] "03 1900"

    sendKeys_ "Enter" (Substring "March")
    sendKeys_ "1111 2222" Unconditional

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Thu, 01 Mar 1900 22:22:00 UTC"

selectTimeBackspace :: TestCase sharedEnv
selectTimeBackspace =
  withTmuxSession' "select time, delete, and re-enter" $ \_ -> do
    startApplication [] "03 1900"

    sendKeys_ "Enter" (Substring "March")
    sendKeys_ "1 2 3 4" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "5" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "4" Unconditional

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Thu, 01 Mar 1900 14:35:00 UTC"

moveSpatiallyVertAcrossMonths :: TestCase sharedEnv
moveSpatiallyVertAcrossMonths =
  withTmuxSession' "move spatially across month boundary" $ \_ -> do
    startApplication ["-d", "-3"] "Jan 2030"

    -- selection: 2030-01-01
    sendKeys_ "Left" (Substring "January")
    -- selection: 2029-12-01
    sendKeys_ "Down" Unconditional
    sendKeys_ "Down" Unconditional
    sendKeys_ "Down" Unconditional
    sendKeys_ "Down" Unconditional
    -- selection: 2029-12-29
    sendKeys_ "Right" Unconditional
    -- selection: 2030-01-27

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Sun, 27 Jan 2030 00:00:00 UTC"

moveSpatiallyVertBoundary :: TestCase sharedEnv
moveSpatiallyVertBoundary =
  withTmuxSession' "move spatially across month boundary" $ \_ -> do
    startApplication ["-d", "--three"] "nov 2029"

    -- selection: 2029-11-01
    sendKeys_ "Down" (Substring "November")
    -- selection: 2029-11-08
    replicateM_ 3 (sendKeys_ "Right" Unconditional)
    -- selection: 2029-12-02
    replicateM_ 4 (sendKeys_ "Down" Unconditional)
    -- selection: 2029-12-30
    replicateM_ 4 (sendKeys_ "Left" Unconditional)
    replicateM_ 4 (sendKeys_ "Down" Unconditional)
    replicateM_ 4 (sendKeys_ "Down" Unconditional)
    replicateM_ 4 (sendKeys_ "Right" Unconditional)
    replicateM_ 4 (sendKeys_ "Right" Unconditional)
    -- selection: 2029-12-31

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Mon, 31 Dec 2029 00:00:00 UTC"

moveSpatiallyHorizAcrossMonths :: TestCase sharedEnv
moveSpatiallyHorizAcrossMonths =
  withTmuxSession' "move horizontally across months" $ \_ -> do
    startApplication ["-d", "-y"] "jun 2065"

    -- selection: 2065-06-01
    sendKeys_ "Up" (Substring "December")
    -- selection: 2065-03-30
    sendKeys_ "Up" Unconditional
    -- selection: 2065-03-23
    sendKeys_ "Left" Unconditional
    sendKeys_ "Left" Unconditional
    -- selection: 2065-02-28
    replicateM_ 14 (sendKeys_ "Down" Unconditional)
    -- selection: 2065-11-28

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Sat, 28 Nov 2065 00:00:00 UTC"

changeInitialDateSelection :: TestCase sharedEnv
changeInitialDateSelection =
  withTmuxSession' "select a different initial date" $ \_ -> do
    startApplication ["-d", "-s", "\"Fri, 19 Jun 2065 00:00:00 UTC\""] "jun 2065"

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Fri, 19 Jun 2065 00:00:00 UTC"

changeInitialTimeSelection :: TestCase sharedEnv
changeInitialTimeSelection =
  withTmuxSession' "select a different initial date" $ \_ -> do
    startApplication ["-s", "\"Fri, 19 Jun 2065 23:42:00 UTC\""] "jun 2065"

    sendKeys_ "Enter" (Substring "June")
    sendKeys_ "Enter" Unconditional

    captureDate >>= assertDate "Fri, 19 Jun 2065 23:42:00 UTC"

invalidInitialDateSelection :: TestCase sharedEnv
invalidInitialDateSelection =
  withTmuxSession' "select date outside of range" $ \_ -> do
    _ <-
      sendLine
        "datepicker -s \"Fri, 15 May 2065 00:00:00 UTC\" jun 2065"
        (Substring $ fromString "user error")

    captured <- snapshot >> capture
    assertCondition
      (Substring $ fromString "specified date (2065-05-15) is not in displayed range")
      captured

tmuxTests :: TestTree
tmuxTests =
  testTmux'
    [ selectDateSpatially,
      selectDateLogically,
      selectDateWithCustomFormat,
      selectDateMondayWeekstart,
      selectDateTwelveMonths,
      selectTime,
      selectTimeAndOverflow,
      selectTimeBackspace,
      moveSpatiallyVertAcrossMonths,
      moveSpatiallyVertBoundary,
      moveSpatiallyHorizAcrossMonths,
      changeInitialDateSelection,
      changeInitialTimeSelection,
      invalidInitialDateSelection
    ]
