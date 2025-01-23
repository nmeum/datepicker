module Main (main) where

import Test.Tasty
import Tmux (tmuxTests)

main :: IO ()
main =
  defaultMain $
    testGroup "Tests" [tmuxTests]
