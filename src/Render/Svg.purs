-- | A figure as a standalone SVG document (used for the PDF pages).
module Render.Svg (svgString, fmt, printPalette) where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Render.Figure (Figure, Item(..), Role(..), paletteSize)

-- | A number for an SVG attribute: up to 5 decimals, no exponent.
foreign import fmt :: Number -> String

foreign import escapeXml :: String -> String

-- | Colours for new objects on white paper; index 0 is the accent colour.
-- | The page uses the same order (`.c0` … `.c7` in style.css).
printPalette :: Array String
printPalette =
  [ "#b5452a", "#1f6fb2", "#1b8a5a", "#8e44ad", "#c27c0e", "#0f8b8d", "#c2185b", "#5b6b7a" ]

colour :: Role -> String
colour = case _ of
  New k -> fromMaybe "#b5452a" (Array.index printPalette (k `mod` paletteSize))
  Old -> "#3b3b40"
  Aux -> "#9a9aa2"

isNew :: Role -> Boolean
isNew = case _ of
  New _ -> true
  _ -> false

-- | Coordinates are flipped (SVG's y axis points down); stroke widths and
-- | font sizes are relative to the size of the view.
svgString :: Figure -> String
svgString f =
  let
    v = f.view
    w = v.maxX - v.minX
    h = v.maxY - v.minY
    size = max w h
    stroke = size * 0.0035
    dot = size * 0.009
    font = size * 0.026
    px x = fmt x
    py y = fmt (-y)
    weight role = if isNew role then " font-weight=\"bold\"" else ""
    label role text x y =
      if text == "" then ""
      else
        "<text x=\"" <> px x <> "\" y=\"" <> py y <> "\" font-size=\"" <> fmt font
          <> "\" font-family=\"sans-serif\" text-anchor=\"middle\" dominant-baseline=\"middle\" fill=\""
          <> colour role
          <> "\""
          <> weight role
          <> ">"
          <> escapeXml text
          <> "</text>"
    item = case _ of
      ICircle c ->
        "<circle cx=\"" <> px c.cx <> "\" cy=\"" <> py c.cy <> "\" r=\"" <> fmt c.r
          <> "\" fill=\"none\" stroke=\""
          <> colour c.role
          <> "\" stroke-width=\""
          <> fmt (width c.role stroke)
          <> "\""
          <> dash c.role stroke
          <> "/>"
          <> label c.role c.label c.lx c.ly
      ISegment s ->
        "<line x1=\"" <> px s.x1 <> "\" y1=\"" <> py s.y1 <> "\" x2=\"" <> px s.x2 <> "\" y2=\"" <> py s.y2
          <> "\" stroke=\""
          <> colour s.role
          <> "\" stroke-width=\""
          <> fmt (width s.role stroke)
          <> "\""
          <> dash s.role stroke
          <> "/>"
          <> label s.role s.label s.lx s.ly
      IPoint p ->
        let
          fill = if p.given && not (isNew p.role) then "#111114" else colour p.role
        in
          "<circle cx=\"" <> px p.x <> "\" cy=\"" <> py p.y <> "\" r=\"" <> fmt (if p.role == Aux then dot * 0.6 else dot)
            <> "\" fill=\""
            <> fill
            <> "\"/>"
            <>
              if p.label == "" then ""
              else
                "<text x=\"" <> px (p.x + dot * 1.4) <> "\" y=\"" <> py (p.y + dot * 1.4) <> "\" font-size=\"" <> fmt font
                  <> "\" font-family=\"sans-serif\" fill=\""
                  <> fill
                  <> "\""
                  <> weight p.role
                  <> ">"
                  <> escapeXml p.label
                  <> "</text>"
  in
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"" <> joinWith " " (map fmt [ v.minX, -v.maxY, w, h ])
      <> "\" width=\"600\" height=\""
      <> fmt (600.0 * h / w)
      <> "\">"
      <> "<rect x=\""
      <> fmt v.minX
      <> "\" y=\""
      <> fmt (-v.maxY)
      <> "\" width=\""
      <> fmt w
      <> "\" height=\""
      <> fmt h
      <> "\" fill=\"#ffffff\"/>"
      <> joinWith "" (map item f.items)
      <> "</svg>"
  where
  width role s = case role of
    New _ -> s * 1.6
    _ -> s
  dash role s = case role of
    Aux -> " stroke-dasharray=\"" <> fmt (s * 4.0) <> " " <> fmt (s * 3.0) <> "\""
    _ -> ""
