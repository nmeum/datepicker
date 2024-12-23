module UI.Time (TimeView, mkTimeView, drawView, processEvent) where

import Data.Char (digitToInt)
import Data.List (intersperse)
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import Util (makePad)

data TimeView = TimeView
  { rawInput :: [Int], -- TODO: Use NonEmpty
    position :: Int
  }

type ClockGlyph = [[Int]]

digitWidth :: Int
digitWidth = 5

digitHeight :: Int
digitHeight = 5

-- Shamelessly stolen from the tmux clock-mode.
--
-- See: https://github.com/tmux/tmux/blob/3.5a/window-clock.c#L53-L124
clockFont :: [ClockGlyph]
clockFont =
  [ [ [1, 1, 1, 1, 1], -- 0
      [1, 0, 0, 0, 1],
      [1, 0, 0, 0, 1],
      [1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1]
    ],
    [ [0, 0, 0, 0, 1], -- 1
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 2
      [0, 0, 0, 0, 1],
      [1, 1, 1, 1, 1],
      [1, 0, 0, 0, 0],
      [1, 1, 1, 1, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 3
      [0, 0, 0, 0, 1],
      [1, 1, 1, 1, 1],
      [0, 0, 0, 0, 1],
      [1, 1, 1, 1, 1]
    ],
    [ [1, 0, 0, 0, 1], -- 4
      [1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1],
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 5
      [1, 0, 0, 0, 0],
      [1, 1, 1, 1, 1],
      [0, 0, 0, 0, 1],
      [1, 1, 1, 1, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 6
      [1, 0, 0, 0, 0],
      [1, 1, 1, 1, 1],
      [1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 7
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1],
      [0, 0, 0, 0, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 8
      [1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1],
      [1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1]
    ],
    [ [1, 1, 1, 1, 1], -- 9
      [1, 0, 0, 0, 1],
      [1, 1, 1, 1, 1],
      [0, 0, 0, 0, 1],
      [1, 1, 1, 1, 1]
    ],
    [ [0, 0, 0, 0, 0], -- :
      [0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0],
      [0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0]
    ]
  ]

mkTimeView :: TimeView
mkTimeView = TimeView [2, 3, 5, 9] 0

drawGlyph :: ClockGlyph -> I.Image
drawGlyph glyph =
  (I.vertCat $ map drawBlock glyph) I.<|> makePad 1 digitHeight

drawView :: TimeView -> I.Image
drawView TimeView {rawInput = input} =
  let (h, m) = splitAt 2 $ map (\d -> drawGlyph $ clockFont !! d) input
   in I.horizCat h I.<|> colonSep I.<|> I.horizCat m
  where
    colonSep :: I.Image
    colonSep = drawGlyph $ last clockFont

processEvent :: TimeView -> E.Event -> Either (Maybe TimeView) TimeOfDay
processEvent view (E.EvKey key _mods) =
  case key of
    E.KEnter -> Right $ TimeOfDay 23 59 00
    E.KChar c -> Left $ processInput view c
    _ -> Left Nothing
processEvent view (E.EvResize _ _) = Left $ Just view
processEvent _ _ = error "not implemented"

------------------------------------------------------------------------

drawBlock :: [Int] -> I.Image
drawBlock = I.horizCat . map (\i -> I.string (attr i) " ")
  where
    attr i
      | i == 1 = Attr.defAttr `Attr.withBackColor` Attr.cyan
      | otherwise = Attr.defAttr

processInput :: TimeView -> Char -> Maybe TimeView
processInput v c
  | c >= '0' && c <= '9' = Just $ cycleDigits v (digitToInt c)
  | otherwise = Nothing

cycleDigits :: TimeView -> Int -> TimeView
cycleDigits v@TimeView {position = p, rawInput = input} n =
  v { rawInput = newInput, position = (p + 1) `mod` length input }
  where
    newInput :: [Int]
    newInput = zipWith (\e i -> if i == p then n else e) input [0 ..]