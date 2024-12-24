module UI.Time (digitWidth, digitHeight, TimeView, mkTimeView) where

import Data.Char (digitToInt, isDigit)
import Data.Time.Calendar qualified as Cal
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (TimeOfDay))
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util (format, horizCenter, makePad)

data TimeView = TimeView
  { rawInput :: [Int], -- TODO: Use NonEmpty
    position :: Int,
    initTime :: LocalTime
  }

instance View TimeView where
  draw = drawView
  process = processEvent

type ClockGlyph = [[Int]]

digitWidth :: Int
digitWidth = 5

digitHeight :: Int
digitHeight = 5

clockWidth :: Int
clockWidth = digitWidth * 5 + 4

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

mkTimeView :: LocalTime -> TimeView
mkTimeView = TimeView [2, 3, 5, 9] 0

drawView :: TimeView -> I.Image
drawView v@TimeView {initTime = t} =
  let str = format "%-d %B, %Y" t
   in horizCenter clockWidth (I.string Attr.defAttr str)
        I.<-> makePad clockWidth 1
        I.<-> drawClock v

processEvent :: TimeView -> E.Event -> Either (Maybe TimeView) LocalTime
processEvent view (E.EvKey key _mods) =
  case key of
    E.KEnter -> Right $ LocalTime (Cal.ModifiedJulianDay 0) (getTimeOfDay view)
    E.KChar c -> Left $ processInput view c
    _ -> Left Nothing
processEvent view (E.EvResize _ _) = Left $ Just view
processEvent _ _ = error "not implemented"

------------------------------------------------------------------------

drawGlyph :: ClockGlyph -> I.Image
drawGlyph glyph =
  I.vertCat (map drawBlock glyph) I.<|> makePad 1 digitHeight

drawClock :: TimeView -> I.Image
drawClock TimeView {rawInput = input} =
  let (h, m) = splitAt 2 $ map (\d -> drawGlyph $ clockFont !! d) input
   in I.horizCat h I.<|> colonSep I.<|> I.horizCat m
  where
    colonSep :: I.Image
    colonSep = drawGlyph $ last clockFont

drawBlock :: [Int] -> I.Image
drawBlock = I.horizCat . map (\i -> I.string (attr i) " ")
  where
    attr i
      | i == 1 = Attr.defAttr `Attr.withBackColor` Attr.cyan
      | otherwise = Attr.defAttr

getTimeOfDay :: TimeView -> TimeOfDay
getTimeOfDay TimeView {rawInput = input} =
  let (h, m) = splitAt 2 input
   in TimeOfDay (toInt h) (toInt m) 0
  where
    toInt :: [Int] -> Int
    toInt = read . concatMap show

processInput :: TimeView -> Char -> Maybe TimeView
processInput v c
  | isDigit c = Just $ cycleDigits v (digitToInt c)
  | otherwise = Nothing

cycleDigits :: TimeView -> Int -> TimeView
cycleDigits v@TimeView {position = p, rawInput = input} n =
  v {rawInput = newInput, position = (p + 1) `mod` length input}
  where
    newInput :: [Int]
    newInput = zipWith (\e i -> if i == p then n else e) input [0 ..]
