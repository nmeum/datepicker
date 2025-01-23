{-# LANGUAGE OverloadedStrings #-}

module Tmux (tmuxTests) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.ByteString qualified as B
import Data.List (find, isSuffixOf)
import Data.Maybe (fromJust)
import Data.String (fromString)
import Test.Tasty
import Test.Tasty.HUnit (assertBool)
import Test.Tasty.Tmux

-- Timezone used for the tests, set explicitly to ensure that test results
-- are deterministic, independent of the value of the TZ environment variable.
timezone :: String
timezone = "CET"

-- Header written by datepicker, used to detect application startup.
--
-- Note: This is only a substring of the header so that it works with -m.
header :: B.ByteString
header = "Tu We Th Fr Sa"

startApplication ::
  (HasTmuxSession a, MonadReader a m, MonadIO m) =>
  [String] ->
  String ->
  m ()
startApplication args dateInput = do
  let cmd = "datepicker " ++ unwords args ++ " " ++ dateInput
  _ <-
    sendLine
      ("env TZ=" ++ timezone ++ " " ++ cmd)
      (Substring header)
  pure ()

captureDate :: (HasTmuxSession a, MonadReader a m, MonadState Capture m, MonadIO m) => m String
captureDate = do
  _ <-
    waitForCondition
      (Substring $ fromString timezone)
      defaultRetries
      defaultBackoff

  out <- captureString <$> (snapshot >> capture)
  pure $ fromJust $ find (isSuffixOf timezone) (reverse $ lines out)

assertDate :: (MonadIO m) => String -> String -> m ()
assertDate expected actual = do
  liftIO $
    assertBool
      ( "Date does not match: "
          ++ expected
          ++ " <-> "
          ++ actual
      )
      (expected == actual)

sendKeys_ ::
  (HasTmuxSession a, MonadReader a m, MonadIO m) =>
  String -> Condition -> m ()
sendKeys_ s c = void (sendKeys s c)

-----------------------------------------------------------------------

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
    captureDate >>= assertDate "Mon, 23 Dec 2024 00:00:00 CET"

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
    captureDate >>= assertDate "Sat, 08 Feb 2020 00:00:00 CET"

selectDateWithCustomFormat :: TestCase sharedEnv
selectDateWithCustomFormat =
  withTmuxSession' "custom date format" $ \_ -> do
    startApplication ["--date-only", "--format", "'%0Y%m%d %Z'"] "July 1998"

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "19980701 CET"

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
    captureDate >>= assertDate "Mon, 06 Jan 2025 00:00:00 CET"

selectTime :: TestCase sharedEnv
selectTime =
  withTmuxSession' "select date and time" $ \_ -> do
    startApplication [] "03 1900"

    sendKeys_ "Enter" Unconditional
    sendKeys_ "2342" Unconditional
    sendKeys_ "Enter" Unconditional

    captureDate >>= assertDate "Thu, 01 Mar 1900 23:42:00 CET"

selectTimeAndOverflow :: TestCase sharedEnv
selectTimeAndOverflow =
  withTmuxSession' "select time, delete, and re-enter" $ \_ -> do
    startApplication [] "03 1900"

    sendKeys_ "Enter" (Substring $ fromString "March")
    sendKeys_ "1111 2222" Unconditional

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Thu, 01 Mar 1900 22:22:00 CET"

selectTimeBackspace :: TestCase sharedEnv
selectTimeBackspace =
  withTmuxSession' "select time, delete, and re-enter" $ \_ -> do
    startApplication [] "03 1900"

    sendKeys_ "Enter" (Substring $ fromString "March")
    sendKeys_ "1 2 3 4" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "5" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "c-h" Unconditional
    sendKeys_ "4" Unconditional

    sendKeys_ "Enter" Unconditional
    captureDate >>= assertDate "Thu, 01 Mar 1900 14:35:00 CET"

tmuxTests :: TestTree
tmuxTests =
  testTmux'
    [ selectDateSpatially,
      selectDateLogically,
      selectDateWithCustomFormat,
      selectDateMondayWeekstart,
      selectTime,
      selectTimeAndOverflow,
      selectTimeBackspace
    ]
