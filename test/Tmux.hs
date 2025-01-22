{-# LANGUAGE OverloadedStrings #-}

module Tmux (tmuxTests) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.State.Class (MonadState)
import qualified Data.ByteString as B
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
header :: B.ByteString
header = "Su Mo Tu We Th Fr Sa"

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

selectDate :: (HasTmuxSession a, MonadReader a m, MonadState Capture m, MonadIO m) => m String
selectDate = do
  -- Submit the current position in the calendar.
  sendKeys_ "Enter" Unconditional
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

selectNextDay :: TestCase sharedEnv
selectNextDay = withTmuxSession' "select next week" $ \_ -> do
  startApplication ["-d"] "03 2020"

  -- selection: 2020-03-01
  sendKeys_ "Right" Unconditional
  -- selection: 2024-03-02

  selectDate >>= assertDate "Mon, 02 Mar 2020 00:00:00 CET"

selectNextWeek :: TestCase sharedEnv
selectNextWeek = withTmuxSession' "select next week" $ \_ -> do
  startApplication ["-d"] "dec 2024"

  -- selection: 2024-12-01
  sendKeys_ "Down" Unconditional
  -- selection: 2024-12-08

  selectDate >>= assertDate "Sun, 08 Dec 2024 00:00:00 CET"

tmuxTests :: TestTree
tmuxTests =
  testTmux'
    [ selectNextDay,
      selectNextWeek
    ]
