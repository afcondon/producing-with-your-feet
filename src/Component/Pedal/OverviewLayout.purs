module Component.Pedal.OverviewLayout
  ( svgAspect
  , bestGrid
  , gridStyle
  , activeGridStyle
  , activeCellStyle
  ) where

import Prelude

import Data.Array (range)
import Data.Foldable (foldl)
import Data.Int (ceil, toNumber)

-- | Portrait aspect ratio of the pedal SVG (320 / 470)
svgAspect :: Number
svgAspect = 320.0 / 470.0

-- | Gap between grid cells in pixels
gap :: Number
gap = 8.0

-- | Find optimal (cols, rows) to pack N items into W×H
-- | maximizing aspect-ratio utilization and fill ratio.
bestGrid :: Number -> Number -> Int -> { cols :: Int, rows :: Int }
bestGrid w h n
  | n <= 0 = { cols: 1, rows: 1 }
  | otherwise =
      let best = foldl pick { cols: 1, rows: n, score: -1.0 } candidates
      in { cols: best.cols, rows: best.rows }
  where
    candidates :: Array Int
    candidates = range 1 n

    pick acc c =
      let
        r = ceil (toNumber n / toNumber c)
        cellW = (w - (toNumber c - 1.0) * gap) / toNumber c
        cellH = (h - (toNumber r - 1.0) * gap) / toNumber r
        cellAspect = cellW / cellH
        utilization = min cellAspect svgAspect / max cellAspect svgAspect
        filledRatio = toNumber n / toNumber (c * r)
        score = utilization * filledRatio
      in
        if score > acc.score
          then { cols: c, rows: r, score }
          else acc

-- | Inline style for uniform grid (no active pedal)
gridStyle :: Number -> Number -> Int -> String
gridStyle w h n =
  let { cols, rows } = bestGrid w h n
  in "display:grid; grid-auto-flow:dense; gap:8px;"
    <> " grid-template-columns:repeat(" <> show cols <> ",1fr);"
    <> " grid-template-rows:repeat(" <> show rows <> ",1fr);"
    <> " width:100%; height:100%;"

-- | Inline style for grid with active pedal in left column
activeGridStyle :: Number -> Number -> Int -> String
activeGridStyle w h n =
  let
    -- Active pedal gets up to half the width, constrained by aspect ratio
    rawActiveW = min (h * svgAspect) (w * 0.5)
    activeW = max rawActiveW 100.0  -- floor to avoid collapse
    remainW = w - activeW - gap
    remainN = n - 1
    { cols: thumbCols, rows: thumbRows } = bestGrid remainW h (max remainN 1)
  in "display:grid; grid-auto-flow:dense; gap:8px;"
    <> " grid-template-columns:" <> show activeW <> "px repeat(" <> show thumbCols <> ",1fr);"
    <> " grid-template-rows:repeat(" <> show thumbRows <> ",1fr);"
    <> " width:100%; height:100%;"

-- | Inline style for the active cell (spans full left column)
activeCellStyle :: String
activeCellStyle = "grid-column:1; grid-row:1/-1;"
