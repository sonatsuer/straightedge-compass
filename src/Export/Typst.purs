-- | The proof as a Typst document with one page per `snapshot` (plus the
-- | final piece). Each page shows the figure at its last step, with the
-- | objects new on the page in the colours of `Render.Figure.newColours`,
-- | followed by the page's steps: source line, notes (Typst markup), new
-- | objects with exact values, and checks. The first page opens with the
-- | title, the given points and the claims; the last ends with the summary.
-- |
-- | Every page is laid out by `fitpage` (in the preamble) to fit one sheet:
-- | it tries smaller figures, then two balanced columns of steps, and only
-- | then scales the page down. Long radicands are named (ρ₁, ρ₂, …) where
-- | they first appear, so that no unbreakable formula is wider than a line.
module Export.Typst (document, preamble, lit, txt, rawInline) where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Proof.Eval (AssertInfo, Def(..), ObjInfo, Page, Step, StepKind(..), Trace, isGeneral, pages)
import Proof.Object (Obj(..))
import Proof.Predicates (detailTypst)
import Render.Describe (describeTypst, objValues, pointTypst)
import Render.Figure (Viewport, figure, newColours, viewport)
import Render.Pretty (Radicals, nameRadicals, noRadicals, typstWidth)
import Render.Svg (printPalette, svgString)

foreign import escape :: String -> String

-- | A Typst string literal.
lit :: String -> String
lit s = "\"" <> escape s <> "\""

-- | Text inserted in markup without interpretation.
txt :: String -> String
txt s = "#" <> lit s

rawInline :: String -> String
rawInline s = "#raw(" <> lit s <> ")"

preamble :: String -> String
preamble title = joinWith "\n"
  [ "#set document(title: " <> lit title <> ")"
  , "#set page(paper: \"a4\", margin: (x: 2.2cm, y: 2cm), numbering: \"1 / 1\", number-align: center)"
  , "#set text(font: \"New Computer Modern\", size: 11pt, lang: \"en\")"
  , "#set par(justify: true)"
  , "#show raw: set text(font: \"DejaVu Sans Mono\", size: 9pt)"
  , "#let good = rgb(\"#2f7d4f\")"
  , "#let pal = (" <> joinWith ", " (map (\c -> "rgb(" <> lit c <> ")") printPalette) <> ")"
  , "#let ok = text(fill: good)[✓]"
  , "#let fig(svg, h) = align(center, box(stroke: 0.5pt + luma(220), inset: 4pt, image(bytes(svg), format: \"svg\", height: h)))"
  , "#let stepline(n, body) = block(above: 0em, below: 0.5em)[#text(fill: luma(120), size: 9pt)[#n.] #h(0.3em) #body]"
  , "#let check(body) = block(above: 0.4em, below: 0.4em)[#ok #body]"
  , "#let where(body) = block(above: 0.4em, below: 0.4em)[#text(fill: luma(90))[where] #body]"
  , "// Lines are measured by their glyphs so that tall radicals cannot collide."
  , "#let note(body) = block(above: 0.5em, below: 0.5em)[#set par(justify: true)\n#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n#body]"
  , "#let item(body) = block(width: 100%, above: 0.85em, below: 0em)[#set par(justify: false, leading: 0.5em)\n#set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n#body]"
  , ""
  , "// Items in two columns, split where the taller column is shortest."
  , "#let colsplit(items, w, gap) = {"
  , "  let hs = items.map(i => measure(block(width: (w - gap) / 2, i)).height)"
  , "  let total = hs.sum(default: 0pt)"
  , "  let best = 0"
  , "  let bestv = total"
  , "  let acc = 0pt"
  , "  for (k, h) in hs.enumerate() {"
  , "    acc += h"
  , "    let v = calc.max(acc, total - acc)"
  , "    if v < bestv { best = k + 1; bestv = v }"
  , "  }"
  , "  grid(columns: (1fr, 1fr), column-gutter: gap,"
  , "    block(width: 100%, items.slice(0, best).join()),"
  , "    block(width: 100%, items.slice(best).join()))"
  , "}"
  , ""
  , "// One page of the proof on one sheet: the first layout that fits, or the"
  , "// last one scaled down."
  , "#let fitpage(head, svg, items, tail) = layout(size => {"
  , "  let make(h, c, w) = {"
  , "    head"
  , "    fig(svg, h)"
  , "    if c == 1 { items.join() } else { colsplit(items, w, 1.4em) }"
  , "    tail"
  , "  }"
  , "  let tall(o, w) = measure(block(width: w, make(o.at(0), o.at(1), w))).height"
  , "  let room = size.height - 2pt"
  , "  let tries = ((10.5cm, 1), (8.5cm, 1), (7cm, 1), (8.5cm, 2), (7cm, 2), (5cm, 2))"
  , "  let fits = tries.find(o => tall(o, size.width) <= room)"
  , "  if fits != none {"
  , "    block(width: size.width, make(fits.at(0), fits.at(1), size.width))"
  , "  } else {"
  , "    let o = tries.last()"
  , "    let f = room / tall(o, size.width)"
  , "    for i in range(8) {"
  , "      let got = f * tall(o, size.width / f)"
  , "      if got <= room { break }"
  , "      f = f * room / got * 0.995"
  , "    }"
  , "    scale(x: f * 100%, y: f * 100%, reflow: true, origin: top + left,"
  , "      block(width: size.width / f, make(o.at(0), o.at(1), size.width / f)))"
  , "  }"
  , "})"
  , ""
  ]

document :: Trace -> String
document t =
  let
    title = fromMaybe "Construction proof" t.title
    view = viewport t
    ps = pages t
    claims = Array.filter (\a -> Array.null a.context) t.asserts
    lastIndex = Array.length ps - 1
    rad = radicals t
  in
    joinWith "\n"
      ( [ preamble title ]
          <> Array.mapWithIndex
            ( \i p ->
                (if i == 0 then "" else "#pagebreak()\n")
                  <> pageBody t rad view p
                    (if i == 0 then titleBlock t rad.names title claims else "")
                    (if i == lastIndex then summary t claims else "")
            )
            ps
      )

-- | Radicand names for the whole document, numbered in order of first
-- | appearance, and the definitions each step introduces.
radicals :: Trace -> { names :: Map String String, introduced :: Map Int (Array { name :: String, radicand :: String }) }
radicals t =
  let
    stepValues s =
      Array.concatMap (\i -> maybe' (Map.lookup i t.objects) (objValues <<< _.obj)) s.objects
        <> Array.concatMap (map _.value <<< _.detail) s.verdicts
    go acc s =
      let
        before = Array.length acc.rs.defs
        rs = foldl nameRadicals acc.rs (stepValues s)
      in
        { rs, introduced: Map.insert s.index (Array.drop before rs.defs) acc.introduced }
    result = foldl go { rs: noRadicals :: Radicals, introduced: Map.empty } t.steps
  in
    { names: result.rs.names, introduced: result.introduced }
  where
  maybe' m f = case m of
    Just v -> f v
    Nothing -> []

titleBlock :: Trace -> Map String String -> String -> Array AssertInfo -> String
titleBlock t names title claims = joinWith "\n"
  ( [ "#align(center)[#text(18pt, weight: \"bold\")[" <> txt title <> "]]"
    , "#v(0.4em)"
    ]
      <> map (\n -> "#note[" <> n <> "]") t.intro
      <>
        [ "*Given:* " <> joinWith ", " (map given givens) <> "."
        , ""
        , "*Claims:* " <> (if Array.null claims then txt "none" else joinWith ", " (map (\c -> rawInline c.text) claims)) <> "."
        , ""
        , txt (generality t)
        , "#v(0.6em)"
        ]
  )
  where
  givens = Array.filter isGiven (Array.sortWith _.id (Array.fromFoldable (Map.values t.objects)))
  given o = case o.obj of
    OPoint p -> txt o.name <> " = " <> pointTypst names p
    _ -> ""

isGiven :: ObjInfo -> Boolean
isGiven o = case o.def of
  DGiven _ -> true
  _ -> false

generality :: Trace -> String
generality t =
  if isGeneral t then
    "There are " <> show t.givens <> " given points. Every straightedge-and-compass construction commutes "
      <> "with similarities of the plane, so checking this configuration proves the claims for every choice of the given points."
  else
    "There are " <> show t.givens <> " given points: the exact checks prove the claims for this configuration of them."

pageBody
  :: Trace
  -> { names :: Map String String, introduced :: Map Int (Array { name :: String, radicand :: String }) }
  -> Viewport
  -> Page
  -> String
  -> String
  -> String
pageBody t rad view p head tail =
  let
    n = Array.length t.steps
    range = { upTo: p.last, newFrom: p.first, expanded: false }
    colours = newColours t range
    heading =
      if p.first == p.last then "== Step " <> show (p.first + 1) <> " of " <> show n
      else "== Steps " <> show (p.first + 1) <> "–" <> show (p.last + 1) <> " of " <> show n
    steps = Array.mapMaybe (Array.index t.steps) (p.first .. p.last)
  in
    joinWith "\n"
      ( [ "#fitpage("
        , "[" <> head <> "\n" <> heading <> "\n],"
        , lit (svgString (figure t view range)) <> ","
        , "("
        ]
          <> map (\s -> "[#item[" <> stepBlock t rad colours s <> "]],") steps
          <>
            [ "),"
            , "[" <> tail <> "],"
            , ")"
            ]
      )

stepBlock
  :: Trace
  -> { names :: Map String String, introduced :: Map Int (Array { name :: String, radicand :: String }) }
  -> Map Int Int
  -> Step
  -> String
stepBlock t rad colours s =
  joinWith "\n"
    ( [ "#stepline(" <> show (s.index + 1) <> ")[" <> rawInline s.text <> inlineCheck <> "]" ]
        <> map (\n -> "#note[" <> n <> "]") s.notes
        <> Array.mapMaybe object s.objects
        <> (if oneCheck then [] else map (\v -> "#check[" <> rawInline v.text <> detail v <> "]") s.verdicts)
        <> map definition (fromMaybe [] (Map.lookup s.index rad.introduced))
    )
    <> "\n"
  where
  -- an assertion is checked on its own line
  oneCheck = s.kind == SAssert && Array.length s.verdicts == 1 && Array.null s.objects
  inlineCheck = case s.verdicts of
    [ v ] | oneCheck -> " #h(0.4em) #ok" <> detail v
    _ -> ""
  detail v = if Array.null v.detail then "" else " $" <> detailTypst rad.names v.detail <> "$"
  object i = Map.lookup i t.objects <#> \o ->
    let
      fill = case Map.lookup i colours of
        Just k -> "pal.at(" <> show k <> ")"
        Nothing -> "black"
    in
      "- #text(fill: " <> fill <> ", weight: \"bold\")[" <> txt o.name <> "]: " <> describeTypst rad.names o.obj
  -- a long radicand is defined by its square, which can break across lines
  definition d =
    if typstWidth d.radicand <= 70 then "#where[$" <> d.name <> " = sqrt(" <> d.radicand <> ")$]"
    else "#where[$" <> d.name <> " > 0$, $" <> d.name <> "^2 = " <> d.radicand <> "$]"

summary :: Trace -> Array AssertInfo -> String
summary t claims = joinWith "\n"
  [ "#v(1em)"
  , "== Summary"
  , if Array.null claims then txt "There are no claims."
    else "#ok *All " <> show (Array.length claims) <> " claims hold:* " <> joinWith ", " (map (\c -> rawInline c.text) claims) <> "."
  , ""
  , txt (generality t)
  , ""
  , "#align(right)[∎]"
  ]
