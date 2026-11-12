-- | The figure as Halogen SVG (styled by CSS classes, so it follows the
-- | page theme). New objects get the colour classes `c0` … `c7`, the same
-- | order as the PDF's palette.
module App.FigureView (figureView) where

import Prelude

import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Properties as HP
import Render.Figure (Figure, Item(..), Role(..), paletteSize)
import Render.Svg (fmt)

svgNS :: Namespace
svgNS = Namespace "http://www.w3.org/2000/svg"

el :: forall w i. String -> Array (HP.IProp () i) -> Array (HH.HTML w i) -> HH.HTML w i
el name = HH.elementNS svgNS (ElemName name)

at :: forall i. String -> String -> HP.IProp () i
at k v = HP.attr (AttrName k) v

roleClass :: Role -> String
roleClass = case _ of
  New k -> "new c" <> show (k `mod` paletteSize)
  Old -> "old"
  Aux -> "aux"

figureView :: forall w i. Figure -> HH.HTML w i
figureView f =
  let
    v = f.view
    w = v.maxX - v.minX
    h = v.maxY - v.minY
    size = max w h
    dot = size * 0.009
    font = size * 0.026
    label role text x y =
      if text == "" then []
      else
        [ el "text"
            [ at "class" ("fig-label " <> roleClass role)
            , at "x" (fmt x)
            , at "y" (fmt (-y))
            , at "font-size" (fmt font)
            , at "text-anchor" "middle"
            , at "dominant-baseline" "middle"
            ]
            [ HH.text text ]
        ]
    item = case _ of
      ICircle c ->
        el "g" []
          ( [ el "circle"
                [ at "class" ("fig-curve " <> roleClass c.role)
                , at "cx" (fmt c.cx)
                , at "cy" (fmt (-c.cy))
                , at "r" (fmt c.r)
                ]
                []
            ]
              <> label c.role c.label c.lx c.ly
          )
      ISegment s ->
        el "g" []
          ( [ el "line"
                [ at "class" ("fig-curve " <> roleClass s.role)
                , at "x1" (fmt s.x1)
                , at "y1" (fmt (-s.y1))
                , at "x2" (fmt s.x2)
                , at "y2" (fmt (-s.y2))
                ]
                []
            ]
              <> label s.role s.label s.lx s.ly
          )
      IPoint p ->
        el "g" [ at "class" ("fig-point " <> roleClass p.role <> (if p.given then " given" else "")) ]
          ( [ el "circle" [ at "cx" (fmt p.x), at "cy" (fmt (-p.y)), at "r" (fmt (if p.role == Aux then dot * 0.6 else dot)) ] [] ]
              <>
                if p.label == "" then []
                else
                  [ el "text"
                      [ at "x" (fmt (p.x + dot * 1.4))
                      , at "y" (fmt (-(p.y + dot * 1.4)))
                      , at "font-size" (fmt font)
                      ]
                      [ HH.text p.label ]
                  ]
          )
  in
    el "svg"
      [ at "class" "figure-svg"
      , at "viewBox" (joinWith " " (map fmt [ v.minX, -v.maxY, w, h ]))
      , at "preserveAspectRatio" "xMidYMid meet"
      , at "role" "img"
      , at "aria-label" "Construction figure"
      ]
      (map item f.items)
