{-# LANGUAGE OverloadedStrings #-}

module Util where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.ByteString qualified as B
import Data.List (find, isSuffixOf)
import Data.Maybe (fromJust)
import Data.String (fromString)
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
  -- Larger window size is needed for test using the '-y' or '-3' option.
  _ <-
    sendLine
      ("tmux resize-window -x 512 -y 512")
      Unconditional

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
