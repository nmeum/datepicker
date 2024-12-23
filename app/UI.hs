module UI (View (..)) where

import Data.Time.LocalTime (LocalTime)
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E

class View a where
  draw :: a -> I.Image
  process :: a -> E.Event -> Either (Maybe a) LocalTime
