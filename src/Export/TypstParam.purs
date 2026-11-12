-- | The Typst document of a universal statement (a script with variables).
-- |
-- | Page 1 states the theorem: the variables, the hypotheses (assumed, and
-- | computed from the construction, each with its step) and, case by case,
-- | the claims. Then come the pages of the proof: the steps before the first
-- | `cases`, and a section for every case, each split into pages at its
-- | snapshots. Each page's figure is the configuration of the samples in
-- | force at its last step, drawn exactly; the text gives the values as
-- | functions of the variables.
module Export.TypstParam (document) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Export.Typst (lit, preamble, rawInline, txt)
import Proof.Eval (StepKind(..), evaluateAt)
import Constructible.ParamField as PF
import Constructible.Poly as P
import Constructible.RatFun as RF
import Proof.Prove (Hyp, HypOrigin(..), PNode(..), PStep, Proof, SObj(..), namesOf)
import Proof.Signs (Rel(..))
import Proof.Syntax (SampleValue(..), Script, labelText)
import Render.Figure (figure, newColours, viewport)
import Render.Svg (svgString)
import Render.Sym (Names)
import Render.Sym as Sym

-- | A segment of the proof: the steps of one node, with its heading.
type Segment = { heading :: String, node :: PNode }

document :: Script -> Proof -> String
document script proof =
  let
    title = fromMaybe "Construction proof" proof.title
    segs = segments proof.root
    pages = Array.concatMap (\s -> map (\p -> { seg: s, steps: p }) (splitPages s.node)) segs
    lastIndex = Array.length pages - 1
  in
    joinWith "\n"
      ( [ preamble title ]
          <> Array.mapWithIndex
            ( \i pg ->
                (if i == 0 then "" else "#pagebreak()\n")
                  <> page script pg.seg pg.steps (isFirstOf pg i pages)
                    (if i == 0 then theoremBlock proof title else "")
                    (if i == lastIndex then "#v(1em)\n#align(right)[∎]" else "")
            )
            pages
      )
  where
  isFirstOf pg i pages = case Array.index pages (i - 1) of
    Just prev -> prev.seg.heading /= pg.seg.heading
    Nothing -> true

-- | The trunk and every case, depth first.
segments :: PNode -> Array Segment
segments = go ""
  where
  go heading node@(PNode n) =
    [ { heading, node } ]
      <> case n.split of
        Nothing -> []
        Just sp -> Array.concatMap
          ( \b -> Array.concatMap
              ( \c -> case c.node of
                  Just nd -> go
                    ( (if heading == "" then "" else heading <> " › ")
                        <> "Case "
                        <> labelText b.label
                        <> (if c.text == "" then "" else " (" <> c.text <> ")")
                    )
                    nd
                  Nothing -> []
              )
              b.cases
          )
          sp.branches

-- | The steps of a node, split at its snapshots.
splitPages :: PNode -> Array (Array PStep)
splitPages (PNode n) =
  let
    go acc cur steps = case Array.uncons steps of
      Nothing -> if Array.null cur then acc else Array.snoc acc cur
      Just { head, tail } ->
        let
          cur' = Array.snoc cur head
        in
          if head.snapshot && not (Array.null tail) then go (Array.snoc acc cur') [] tail else go acc cur' tail
  in
    go [] [] n.steps

page :: Script -> Segment -> Array PStep -> Boolean -> String -> String -> String
page script seg steps first head tail =
  let
    PNode n = seg.node
    names = namesOf n.vars
    firstStep = fromMaybe 0 (_.index <$> Array.head steps)
    lastStep = fromMaybe 0 (_.index <$> Array.last steps)
    fig = fromMaybe Map.empty (_.figure <$> Array.last steps)
    trace = evaluateAt fig script
    range = { upTo: lastStep, newFrom: firstStep, expanded: false }
    svg =
      if Array.length trace.steps > lastStep then svgString (figure trace (viewport trace) range)
      else "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 10 10'></svg>"
    colours = newColours trace range
    colourOf name = Map.lookup name trace.scope >>= \id -> Map.lookup id colours
    heading =
      (if first && seg.heading /= "" then "= " <> txt seg.heading <> "\n" <> caseLine seg <> "\n" else "")
        <> "== "
        <> (if firstStep == lastStep then "Step " <> show (firstStep + 1) else "Steps " <> show (firstStep + 1) <> "–" <> show (lastStep + 1))
    sampleLine = "#align(center)[#text(size: 9pt, fill: luma(100))[" <> txt ("drawn for " <> sampleText fig) <> "]]"
  in
    joinWith "\n"
      ( [ "#fitpage("
        , "[" <> head <> "\n" <> heading <> "\n],"
        , lit svg <> ","
        , "("
        ]
          <> [ "[" <> sampleLine <> "]," ]
          <> map (\s -> "[#item[" <> stepBlock names colourOf s <> "]],") steps
          <> [ "),", "[" <> tail <> "],", ")" ]
      )

caseLine :: Segment -> String
caseLine seg =
  let
    PNode n = seg.node
    names = namesOf n.vars
    branchHyps = Array.filter (\h -> h.origin == BranchCond) n.hyps
    subst = map (\s -> s.var <> " = " <> s.value) n.subst
  in
    joinWith "\n"
      ( map (\h -> "#text(fill: luma(80))[" <> txt h.text <> ": $" <> condTypst names h <> "$]") branchHyps
          <> (if Array.null subst then [] else [ "#text(fill: luma(80))[" <> txt ("with " <> joinWith ", " subst) <> "]" ])
      )

sampleText :: Map String SampleValue -> String
sampleText fig = joinWith ", "
  ( map
      ( \(Tuple k v) -> k <> " = " <> case v of
          SVNum q -> show q
          SVPoint p -> "(" <> show p.x <> ", " <> show p.y <> ")"
      )
      (Map.toUnfoldable fig :: Array (Tuple String SampleValue))
  )

-- | A hypothesis as Typst math; `v != c` when it is linear in one variable.
condTypst :: Names -> Hyp -> String
condTypst names h = case P.vars h.poly, h.rel of
  [ v ], RNonZero | P.degIn h.poly v == 1 ->
    let
      cs = P.coeffsIn h.poly v
      a = fromMaybe zero (Array.index cs 1 >>= P.constValue)
      b = fromMaybe zero (Array.index cs 0 >>= P.constValue)
    in
      names.typst v <> " != " <> Sym.ratTypst names (RF.fromRational (negate b / a))
  _, rel -> Sym.polyTypst names h.poly <> " " <> case rel of
    RPos -> "> 0"
    RNeg -> "< 0"
    RNonZero -> "!= 0"
    RNonNeg -> ">= 0"
    RNonPos -> "<= 0"

stepBlock :: Names -> (String -> Maybe Int) -> PStep -> String
stepBlock names colourOf s =
  joinWith "\n"
    ( [ "#stepline(" <> show (s.index + 1) <> ")[" <> rawInline s.text <> inlineCheck <> "]" ]
        <> map (\n -> "#note[" <> n <> "]") s.notes
        <> map object s.objects
        <> map hyp s.hyps
        <> (if oneCheck then [] else map (\v -> "#check[" <> rawInline v.text <> " — " <> txt v.why <> "]") s.verdicts)
    )
    <> "\n"
  where
  oneCheck = (s.kind == SAssert || s.kind == SAssume) && Array.length s.verdicts == 1 && Array.null s.objects
  inlineCheck = case s.verdicts of
    [ v ] | oneCheck -> " #h(0.4em) #ok " <> txt v.why
    _ -> ""
  object o =
    let
      fill = case colourOf o.name of
        Just k -> "pal.at(" <> show k <> ")"
        Nothing -> "black"
    in
      "- #text(fill: " <> fill <> ", weight: \"bold\")[" <> txt o.name <> "]: " <> value o.obj
  value = case _ of
    SPt p -> "$(" <> Sym.pcTypst names p.x <> ", " <> Sym.pcTypst names p.y <> ")$"
    SLn l -> "$(" <> Sym.pcTypst names l.a <> ") x + (" <> Sym.pcTypst names l.b <> ") y + (" <> Sym.pcTypst names l.c <> ") = 0$"
    SCi c -> "centre $(" <> Sym.pcTypst names c.cx <> ", " <> Sym.pcTypst names c.cy <> ")$, $r^2 = " <> Sym.pcTypst names c.r2 <> "$"
  hyp h =
    let
      kind = case h.origin of
        Computed -> "needs"
        Assumed -> "assumes"
        BranchCond -> "case"
    in
      "#text(fill: luma(90))[" <> kind <> ":] " <> txt h.text <> ": $" <> condTypst names h <> "$"

-- | Page 1: the theorem.
theoremBlock :: Proof -> String -> String
theoremBlock proof title =
  let
    PNode root = proof.root
    names = namesOf root.vars
    trunkHyps = root.theorem
    givens = Array.filter (\s -> s.kind == SGiven) root.steps
    fixed = Array.filter (\s -> Array.all constantPoint s.objects) givens
    params = Array.filter (\v -> isNothing v.free) root.vars
    frees = Array.filter (\v -> isJust v.free) root.vars
    claimsOf (PNode n) = map (\v -> rawInline v.text) (Array.concatMap (\s -> if s.kind == SAssert then s.verdicts else []) n.steps)
    trunkClaims = claimsOf proof.root
    newHyps (PNode n) = Array.filter (\h -> not (Array.any (\t -> t.poly == h.poly && t.rel == h.rel) trunkHyps)) n.theorem
    hypItem ns h = "- " <> txt (h.text <> ": ") <> "$" <> condTypst ns h <> "$" <> (if h.origin == Computed then " " <> txt "(needed by the construction)" else "")
    caseItems = case root.split of
      Nothing -> []
      Just sp ->
        [ "", "Then, according to " <> rawInline sp.text <> ":" ]
          <> Array.concatMap
            ( \b -> case b.impossible of
                Just why -> [ "- " <> rawInline (labelText b.label) <> ": " <> txt ("impossible (" <> why <> ")") ]
                Nothing -> Array.mapMaybe
                  ( \c -> c.node <#> \nd@(PNode n) ->
                      let
                        ns = namesOf n.vars
                        claims = claimsOf nd
                        extra = newHyps nd
                      in
                        "- " <> rawInline (labelText b.label <> (if c.text == "" then "" else " (" <> c.text <> ")")) <> ": "
                          <> (if Array.null extra then "" else txt "if " <> joinWith ", " (map (\h -> "$" <> condTypst ns h <> "$") extra) <> txt ", then ")
                          <> (if Array.null claims then txt "no claims" else joinWith ", " claims)
                          <> "."
                  )
                  b.cases
            )
            sp.branches
  in
    joinWith "\n"
      ( [ "#align(center)[#text(18pt, weight: \"bold\")[" <> txt title <> "]]", "#v(0.4em)" ]
          <> map (\n -> "#note[" <> n <> "]") proof.intro
          <>
            [ "*Variables:* "
                <> joinWith ", " (map (\v -> "$" <> v.typst <> "$") params)
                <> (if Array.null params then "" else txt " (real numbers)")
                <> joinWith "" (map (\v -> txt "; " <> "$" <> v.typst <> "$" <> txt (" places " <> v.name <> " on " <> maybe' v.free _.object "")) frees)
                <> "."
            , ""
            , "*Given:* " <> joinWith ", " (Array.concatMap (\s -> map (\o -> txt o.name <> " = " <> pointValue names o.obj) s.objects) givens) <> "."
            , ""
            , "*Theorem.* For all values of the variables"
                <> (if Array.null trunkHyps then "" else " such that")
            ]
          <> map (hypItem names) trunkHyps
          <> (if Array.null trunkClaims then [] else [ "", txt "the claims hold: " <> joinWith ", " trunkClaims <> "." ])
          <> caseItems
          <>
            [ ""
            , if Array.length fixed == 2 && Array.length givens > 2 || Array.length fixed == 2 && not (Array.null root.vars) then
                txt "Two given points are fixed; every configuration is similar to one with these two points, so the statement holds for all positions of the given points."
              else ""
            , "#v(0.6em)"
            ]
      )
  where
  constantPoint o = case o.obj of
    SPt p -> constant p.x && constant p.y
    _ -> false
  constant x = Array.all (Array.null <<< RF.vars) (PF.baseElements x)
  pointValue names = case _ of
    SPt p -> "$(" <> Sym.pcTypst names p.x <> ", " <> Sym.pcTypst names p.y <> ")$"
    _ -> ""
  maybe' m f d = case m of
    Just v -> f v
    Nothing -> d
