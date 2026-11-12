-- | The Help dialog: a reference of the script language and the page.
module App.Help (helpView) where

import Prelude

import App.Editor (tokensOf)
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen as H
import Web.Event.Event (Event, EventType(..))

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< H.ClassName

-- | `close` closes the dialog; `backdrop` receives clicks on the backdrop.
helpView :: forall w i. i -> (Event -> i) -> HH.HTML w i
helpView close backdrop =
  HH.div [ cls "modal-backdrop", HE.handler (EventType "click") backdrop ]
    [ HH.div
        [ cls "modal"
        , HP.attr (AttrName "role") "dialog"
        , HP.attr (AttrName "aria-modal") "true"
        , HP.attr (AttrName "aria-labelledby") "help-title"
        ]
        [ HH.div [ cls "modal-head" ]
            [ HH.h2 [ HP.id "help-title" ] [ HH.text "Help" ]
            , HH.button [ cls "control icon", HP.title "Close (Esc)", HE.onClick \_ -> close ] [ HH.text "×" ]
            ]
        , HH.div [ cls "modal-body" ]
            [ section "Statements"
                [ row "given A = (0, 0)" "a given point with rational coordinates, e.g. (1/2, -3)"
                , row "l = line A B" "the line through two distinct points"
                , row "c = circle O A" "the circle with centre O through A"
                , row "X = meet a b" "the intersection of two lines or circles, when it is one point"
                , row "X, Y = meet a b" "both intersection points, in lexicographic order (x, then y)"
                , row "X = meet a b SELECTOR" "one of two intersection points (see below)"
                , row "M = midpoint(A, B)" "a call of a construction"
                , row "assert PREDICATE" "a claim, checked with exact arithmetic"
                , row "note \"…\"" "text for the step above (Typst markup in the PDF)"
                , row "snapshot" "a PDF page ends here"
                , row "show segment A B" "draw the segment AB (drawing only: no object, no step)"
                , row "show polygon A B C …" "draw the closed polygon, e.g. a triangle"
                , row "show circle O A" "draw the circle with centre O through A"
                , row "show circle through A B C" "draw the circle through three points"
                , row "title \"…\"" "the title of the proof"
                , row "-- …" "a comment"
                ]
            , section "Universal statements"
                [ row "param p ~ 1/3" "a real variable; 1/3 is the value drawn"
                , row "given C = (p, q)" "a given point with variable coordinates"
                , row "P = point on u ~ (0, 1)" "any point of the line or circle u, drawn near (0, 1)"
                , row "assume ccw A B C" "a hypothesis (ccw, dist comparisons, !=, or an equation)"
                , row "cases meet u k" "split: branches none:, one T:, two T1, T2:, same:"
                , row "cases side P A B" "split: left:, on:, right:"
                , row "cases compare dist A B, dist C D" "split: less:, equal:, greater:"
                , row "cases P == Q, cases PREDICATE" "split: equal:/different:, or yes:/no:"
                , row "end" "closes the cases; later statements continue in every branch"
                , row "sample p ~ 2/3" "other values to draw from here on"
                , row "snapshot p ~ 2/3" "a page break, and other values from here on"
                , HH.p_ [ HH.text "A script with variables is proved for all values at once. The tool computes the hypotheses the construction needs (for example that two circles meet), proves equalities as identities, and decides signs from the hypotheses. When a choice depends on the values, split with cases; a branch you leave out must be impossible. Every case is drawn for its sample values, which are checked against its hypotheses. Use the sliders (variables, and the angle or position of each free point), or drag the points, to see other configurations." ]
                ]
            , section "Selectors"
                [ row "except P" "the point other than P"
                , row "nearest P, farthest P" "by the distance from P (a name or a point like (0, 1))"
                , row "left A B, right A B" "on that side of the directed line A → B"
                ]
            , section "Predicates"
                [ row "on P x" "P lies on the line or circle x"
                , row "P == Q, P != Q" "equal or distinct points"
                , row "dist A B == dist C D" "also !=, <, <=, >, >="
                , row "collinear A B C …" "the points lie on one line"
                , row "concyclic A B C D …" "the points lie on one circle"
                , row "parallel l m, perpendicular l m" "lines"
                , row "angle A B C == angle D E F" "equal angles (between 0 and π)"
                , row "ccw A B C" "A, B, C turn counterclockwise"
                , row "between A B C" "B lies strictly between A and C"
                ]
            , section "Built-in constructions"
                [ row "perpBisector(A, B)" "the perpendicular bisector of AB"
                , row "midpoint(A, B)" "the midpoint of AB (Euclid I.10)"
                , row "circleDiameter(A, B)" "the circle with diameter AB"
                , row "perpAt(A, B, P)" "the perpendicular to AB at its point P ≠ A (Euclid I.11)"
                , row "perpFrom(A, B, P)" "the perpendicular from P to AB (Euclid I.12)"
                , row "parallelThrough(A, B, P)" "the parallel to AB through P"
                , row "angleBisector(A, O, B)" "the bisector of the angle AOB (Euclid I.9)"
                , row "transfer(O, A, B)" "the circle with centre O and radius AB (Euclid I.2)"
                ]
            , section "Your own constructions"
                [ HH.pre_ (highlighted "def nextVertex(P, Q, u) -> R:\n  k = circle Q P\n  R = meet k u except P\nend")
                , HH.p_ [ HH.text "Inside a def, only the parameters and earlier local names are visible. A def may call other constructions and contain assertions; it may not contain given, title or snapshot." ]
                ]
            , section "Pages and the PDF"
                [ HH.p_
                    [ HH.text "Each snapshot ends a page; the steps after the last snapshot form the final page, and consecutive snapshots count once. A page shows the figure after its last step; when it adds several objects, each gets its own colour, used for its name too. Use \"by page\" to play the proof page by page. A page that does not fit one sheet gets a smaller figure, then two columns, then smaller type: add a snapshot to split it. Long radicands in exact values are named ρ₁, ρ₂, … and defined where they first appear." ]
                ]
            , section "Notes"
                [ HH.p_
                    [ HH.text "Notes are Typst markup in the PDF: $eta_0 = c_1 + c_4$ is mathematics, *bold* is bold. In a string, \\\" is a quote, \\\\ a backslash and \\n a new line; any other backslash is kept, so Typst escapes such as \\* work." ]
                ]
            , section "Console"
                [ plainRow "a statement" "added to the script when it checks"
                , plainRow "show X" "the exact value of X"
                , plainRow "goto N, steps, undo, clear" "navigate and edit"
                , plainRow "export pdf | typst | lean" "download the proof"
                ]
            , section "Keys and mouse"
                [ plainRow "← →, Home, End" "previous and next step (or page), first, last"
                , plainRow "wheel, + −" "zoom the figure"
                , plainRow "drag" "move the figure"
                , plainRow "double-click, 0" "fit the figure"
                ]
            ]
        ]
    ]
  where
  section title body = HH.section [ cls "help-section" ] ([ HH.h3_ [ HH.text title ] ] <> body)
  -- script syntax, coloured as in the editor
  row code text = HH.div [ cls "help-row" ] [ HH.code_ (highlighted code), HH.span_ [ HH.text text ] ]
  plainRow code text = HH.div [ cls "help-row" ] [ HH.code_ [ HH.text code ], HH.span_ [ HH.text text ] ]
  highlighted code = map token (tokensOf code)
  token t = case t.kind of
    "" -> HH.text t.text
    "variableName" -> HH.text t.text
    kind -> HH.span [ cls ("tok-" <> kind) ] [ HH.text t.text ]
