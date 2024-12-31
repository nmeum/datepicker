module UI.Time (TimeView, mkTimeView) where

import Data.Char (digitToInt, isDigit)
import Data.List.NonEmpty qualified as NE
import Data.Time.Calendar qualified as Cal
import Data.Time.LocalTime (LocalTime (LocalTime), TimeOfDay (todHour, todMin), makeTimeOfDayValid)
import Graphics.Vty.Attributes qualified as Attr
import Graphics.Vty.Image qualified as I
import Graphics.Vty.Input.Events qualified as E
import UI (View (..))
import Util (format, horizCenter, makePad)

data TimeView = TimeView
  { rawInput :: NE.NonEmpty Int,
    position :: Word,
    initTime :: LocalTime
  }

instance View TimeView where
  draw = drawView
  process = processEvent

type ClockGlyph = [[Int]]

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

mkTimeView :: TimeOfDay -> LocalTime -> TimeView
mkTimeView cur = TimeView (NE.fromList $ toInput cur) 0
  where
    toInput :: TimeOfDay -> [Int]
    toInput t = fromInt (todHour t) ++ fromInt (todMin t)

    fromInt :: Int -> [Int]
    fromInt n =
      let t = map digitToInt $ show n
       in if length t < 2 then 0 : t else t

drawView :: TimeView -> I.Image
drawView v@TimeView {initTime = t} =
  let str = format "%-d %B, %Y" t
      clk = drawClock v
   in horizCenter (I.imageWidth clk) (I.string Attr.defAttr str)
        I.<-> makePad (I.imageWidth clk) 1
        I.<-> drawClock v

processEvent :: TimeView -> E.Event -> Either (Maybe TimeView) LocalTime
processEvent view (E.EvKey key _mods) =
  case key of
    E.KChar c -> Left $ processInput view c
    E.KBS -> Left $ Just (moveCursor view (-1))
    E.KLeft -> Left $ Just (moveCursor view (-1))
    E.KRight -> Left $ Just (moveCursor view 1)
    E.KEnter -> case getTimeOfDay view of
      Nothing -> Left Nothing -- TODO: Provide visual feedback
      Just t -> Right $ LocalTime (Cal.ModifiedJulianDay 0) t
    _ -> Left Nothing
processEvent view (E.EvResize _ _) = Left $ Just view
processEvent _ _ = Left Nothing

------------------------------------------------------------------------

moveCursor :: TimeView -> Int -> TimeView
moveCursor view@TimeView {rawInput = input, position = p} off =
  let len = fromIntegral $ NE.length input
   in view {position = (p + fromIntegral off) `mod` len}

drawGlyph :: ClockGlyph -> Attr.Attr -> I.Image
drawGlyph glyph attr =
  let digits = map (`drawBlock` attr) glyph
    in I.vertCat digits I.<|> makePad 1 (length digits)

drawClock :: TimeView -> I.Image
drawClock TimeView {position = curPos, rawInput = input} =
  let (h, m) = NE.splitAt 2 $ NE.zipWith drawDigit (NE.fromList [0 ..]) input
   in I.horizCat h I.<|> colonSep I.<|> I.horizCat m
  where
    defAttr :: Attr.Attr
    defAttr = Attr.defAttr `Attr.withBackColor` Attr.cyan

    drawDigit idx digit =
      drawGlyph (clockFont !! digit) $
        if idx == curPos
          then defAttr `Attr.withBackColor` Attr.magenta
          else defAttr

    colonSep :: I.Image
    colonSep = drawGlyph (last clockFont) defAttr

drawBlock :: [Int] -> Attr.Attr -> I.Image
drawBlock blk attr = I.horizCat $ map (\i -> I.char (a i) ' ') blk
  where
    a i = if i == 1 then attr else Attr.defAttr

getTimeOfDay :: TimeView -> Maybe TimeOfDay
getTimeOfDay TimeView {rawInput = input} =
  let (h, m) = NE.splitAt 2 input
   in makeTimeOfDayValid (toInt h) (toInt m) 0
  where
    toInt :: [Int] -> Int
    toInt = read . concatMap show

processInput :: TimeView -> Char -> Maybe TimeView
processInput v c
  | isDigit c = Just $ cycleDigits v (digitToInt c)
  | otherwise = Nothing

cycleDigits :: TimeView -> Int -> TimeView
cycleDigits v@TimeView {position = p, rawInput = input} n =
  let newView = moveCursor v 1
   in newView {rawInput = newInput}
  where
    newInput :: NE.NonEmpty Int
    newInput = NE.zipWith (\e i -> if i == p then n else e) input (NE.fromList [0 ..])
