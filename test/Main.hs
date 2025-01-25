module Main (main) where

import QuickCheck (quickTests)
import Test.Tasty
import Tmux (tmuxTests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ tmuxTests,
        quickTests
      ]
