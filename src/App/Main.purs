-- | The browser app: script editor, figure with playback, console, help.
module App.Main (component) where

import Prelude

import App.Browser as B
import App.Editor (EditorView)
import App.Editor as Editor
import App.Export as X
import App.FigureView (figureView)
import App.Help (helpView)
import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Number as Number
import Data.String (Pattern(..), joinWith, trim)
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (attempt, delay, message)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Constructible.Field as C
import Constructible.Rational as Q
import Export.Core as Core
import Export.Universal as Universal
import Export.Typst as Typst
import Export.TypstParam as TypstParam
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (AttrName(..), ElemName(..), Namespace(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Proof.Eval (Page, Step, StepKind(..), Trace, Valuation, evaluateAt, isGeneral, pages)
import Proof.Object (Obj(..))
import Proof.Prove (HypOrigin(..), PNode(..), PStep, Proof, hypDisplay, leaves, namesOf, prove, stepsAlong)
import Proof.Syntax (Coord(..), Expr(..), Name, SampleValue(..), Script, Stmt(..), labelText)
import Proof.Parser (ParseError, parseScript, parseStatement)
import Proof.Predicates (detailText)
import Render.Describe (describeObj)
import Render.Figure (Range, Viewport, figure, newColours, panBy, viewport, zoomAt)
import Web.Event.Event (Event, EventType(..))
import Web.HTML (window)
import Web.HTML.Window as Window

data Entry = In String | Out String | Err String

type State =
  { source :: String
  , trace :: Maybe Trace
  , parseError :: Maybe ParseError
  , view :: Viewport -- fits the whole construction
  , zoom :: Maybe Viewport -- the view after zooming or panning; Nothing = fit
  , pointers :: Map Int { fx :: Number, fy :: Number }
  , step :: Int
  , byPage :: Boolean
  , expanded :: Boolean
  , console :: Array Entry
  , input :: String
  , status :: Maybe String
  , editor :: Maybe EditorView
  , examples :: Array B.Example
  , helpOpen :: Boolean
  -- universal statements
  , script :: Maybe Script
  , proof :: Maybe Proof
  , caseIx :: Int -- the case shown when no values are imposed
  , override :: Maybe Valuation -- values set with the sliders or by dragging
  , drag :: Maybe Draggable
  }

-- | A point that can be dragged: a given point with variable coordinates,
-- | or a free point on a line or circle.
type Draggable = { name :: Name, x :: Maybe Name, y :: Maybe Name, free :: Boolean }

data Action
  = Initialize
  | Edited String
  | Go Int
  | Move Int
  | ToggleExpanded
  | ToggleByPage
  | SetInput String
  | Submit Event
  | ChooseExample String
  | Open
  | Opened String String
  | Save
  | ExportPdf
  | ExportTypst
  | ExportLean
  | Key Event
  | Wheel Event
  | PointerDown Event
  | PointerMove Event
  | PointerUp Event
  | ZoomBy Number
  | Fit
  | ShowHelp Boolean
  | HelpBackdrop Event
  | ChooseCase String
  | SetParam Name String
  | SetFree Name Boolean String
  | ResetValues

storageKey :: String
storageKey = "straightedge-compass:draft"

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

consoleRef :: H.RefLabel
consoleRef = H.RefLabel "console-log"

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

initialState :: State
initialState =
  { source: ""
  , trace: Nothing
  , parseError: Nothing
  , view: { minX: -1.0, minY: -1.0, maxX: 1.0, maxY: 1.0 }
  , zoom: Nothing
  , pointers: Map.empty
  , step: 0
  , byPage: false
  , expanded: false
  , console: [ Out "Type a statement (e.g. c = circle A B) or a command ('help' lists them). The Help button explains the language." ]
  , input: ""
  , status: Nothing
  , editor: Nothing
  , examples: []
  , helpOpen: false
  , script: Nothing
  , proof: Nothing
  , caseIx: 0
  , override: Nothing
  , drag: Nothing
  }

stepCount :: State -> Int
stepCount st = maybe 0 (Array.length <<< _.steps) st.trace

currentStep :: State -> Maybe Step
currentStep st = st.trace >>= \t -> Array.index t.steps st.step

pagesOf :: State -> Array Page
pagesOf st = maybe [] pages st.trace

-- | The page containing the current step, and its index.
currentPage :: State -> Maybe { index :: Int, page :: Page }
currentPage st =
  let
    ps = pagesOf st
  in
    Array.findIndex (\p -> p.first <= st.step && st.step <= p.last) ps >>= \i ->
      (\p -> { index: i, page: p }) <$> Array.index ps i

-- | The steps shown and those whose objects are new: the current step, or the
-- | current page.
range :: State -> Range
range st = case st.byPage, currentPage st of
  true, Just { page } -> { upTo: page.last, newFrom: page.first, expanded: false }
  _, _ -> { upTo: st.step, newFrom: st.step, expanded: st.expanded }

effectiveView :: State -> Viewport
effectiveView st = fromMaybe st.view st.zoom

-------------------------------------------------------------------------------
-- Evaluation

-- | Re-evaluates the source. A parse error keeps the previous trace on screen.
-- | A script with variables is also proved, and drawn for the chosen case
-- | (or for the values set with the sliders).
recompute :: String -> State -> State
recompute src st = case parseScript src of
  Left e -> st { source = src, parseError = Just e }
  Right script ->
    let
      proof = if parametric script then Just (prove script) else Nothing
      cases = maybe 0 (Array.length <<< caseList) proof
      st1 = st { source = src, parseError = Nothing, script = Just script, proof = proof, caseIx = min st.caseIx (max 0 (cases - 1)) }
    in
      reinstance true st1

-- | Evaluates the script at the current values; `refit` fits the view.
reinstance :: Boolean -> State -> State
reinstance refit st = case st.script of
  Nothing -> st
  Just script ->
    let
      t = evaluateAt (valuationOf st) script
      n = Array.length t.steps
      atEnd = st.step >= stepCount st - 1
    in
      st
        { trace = Just t
        , view = if refit then viewport t else st.view
        , step = if atEnd then max 0 (n - 1) else min st.step (max 0 (n - 1))
        }

parametric :: Script -> Boolean
parametric script = Array.any isParam script.body
  where
  isParam s = case s.value of
    Param _ -> true
    Cases _ _ -> true
    GivenParam _ _ _ -> true
    Bind _ (EPointOn _ _) -> true
    _ -> false

-- | The cases of a proof, each with the configuration that shows it.
caseList :: Proof -> Array { label :: String, valuation :: Valuation }
caseList proof = map
  ( \l ->
      { label: if Array.null l.path then "the statement" else joinWith " › " l.path
      , valuation: lastFigure l.node
      }
  )
  (leaves proof.root)
  where
  lastFigure (PNode n) = maybe Map.empty _.figure (Array.last n.steps)

-- | The values drawn: those set by the user, or the chosen case's samples.
valuationOf :: State -> Valuation
valuationOf st = case st.override, st.proof of
  Just v, _ -> v
  Nothing, Just p -> maybe Map.empty _.valuation (Array.index (caseList p) st.caseIx)
  Nothing, Nothing -> Map.empty

draggables :: Script -> Array Draggable
draggables script = Array.concatMap go script.body
  where
  go s = case s.value of
    GivenParam n cx cy -> [ { name: n, x: param cx, y: param cy, free: false } ]
    Bind [ n ] (EPointOn _ _) -> [ { name: n, x: Nothing, y: Nothing, free: true } ]
    Cases _ bs -> Array.concatMap (\b -> Array.concatMap go b.body) bs
    _ -> []
  param = case _ of
    CParam p -> Just p
    CRat _ -> Nothing

-- | A rational close to x (hundredths).
hundredths :: Number -> Q.Rational
hundredths x = Q.fromInts (Int.round (x * 100.0)) 100

-- | The figure coordinates of a pointer position.
atPointer :: State -> { fx :: Number, fy :: Number } -> { x :: Number, y :: Number }
atPointer st p =
  let
    v = effectiveView st
  in
    { x: v.minX + p.fx * (v.maxX - v.minX), y: v.maxY - p.fy * (v.maxY - v.minY) }

-- | The draggable point nearest to a figure position, if it is close.
grab :: State -> { x :: Number, y :: Number } -> Maybe Draggable
grab st pos = do
  script <- st.script
  t <- st.trace
  let
    v = effectiveView st
    reach = 0.04 * (v.maxX - v.minX)
    dist d = do
      id <- Map.lookup d.name t.scope
      o <- Map.lookup id t.objects
      case o.obj of
        OPoint p ->
          let
            dx = C.toNumber p.x - pos.x
            dy = C.toNumber p.y - pos.y
          in
            Just (Tuple d (Number.sqrt (dx * dx + dy * dy)))
        _ -> Nothing
    near = Array.sortWith (\(Tuple _ d) -> d) (Array.mapMaybe dist (draggables script))
  case Array.head near of
    Just (Tuple d r) | r <= reach -> Just d
    _ -> Nothing

-- | Moves a dragged point to a position.
dragTo :: Draggable -> { x :: Number, y :: Number } -> Valuation -> Valuation
dragTo d pos v
  | d.free = Map.insert d.name (SVPoint { x: hundredths pos.x, y: hundredths pos.y }) v
  | otherwise =
      let
        setX = maybe identity (\p -> Map.insert p (SVNum (hundredths pos.x))) d.x
        setY = maybe identity (\q -> Map.insert q (SVNum (hundredths pos.y))) d.y
      in
        setY (setX v)

errorLine :: State -> Int
errorLine st = case st.parseError of
  Just e -> e.line
  Nothing -> case st.proof >>= \p -> Array.head p.errors of
    Just e -> e.line
    Nothing -> fromMaybe 0 (st.trace >>= _.error <#> _.line)

-------------------------------------------------------------------------------
-- Actions

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> do
    { emitter, listener } <- liftEffect HS.create
    void (H.subscribe emitter)
    H.getHTMLElementRef editorRef >>= traverse_' \el -> do
      view <- liftEffect (Editor.create el "" (\text -> HS.notify listener (Edited text)))
      H.modify_ _ { editor = Just view }
    w <- liftEffect window
    void $ H.subscribe (eventListener (EventType "keydown") (Window.toEventTarget w) (Just <<< Key))
    index <- H.liftAff (attempt (B.fetchText "examples/index.json"))
    let
      examples = either' [] B.parseExamples index
    H.modify_ _ { examples = examples }
    src <- initialSource examples
    setSource src
    H.modify_ _ { zoom = Nothing }
    handleAction (Go 0)

  Edited text -> do
    st <- H.get
    when (text /= st.source) do
      H.modify_ (recompute text)
      liftEffect (B.storageSet storageKey text)
      syncEditor false

  Go i -> do
    st <- H.get
    let
      n = stepCount st
      target = max 0 (min (n - 1) i)
      -- in page mode, a step stands for the end of its page
      snapped =
        if st.byPage then maybe target _.last (Array.find (\p -> p.first <= target && target <= p.last) (pagesOf st))
        else target
    H.modify_ _ { step = snapped }
    syncEditor true

  Move d -> do
    st <- H.get
    case st.byPage, currentPage st of
      true, Just { index } ->
        for_ (Array.index (pagesOf st) (max 0 (index + d))) \p -> handleAction (Go p.last)
      _, _ -> handleAction (Go (st.step + d))

  ToggleExpanded -> H.modify_ \st -> st { expanded = not st.expanded }

  ToggleByPage -> do
    H.modify_ \st -> st { byPage = not st.byPage }
    st <- H.get
    handleAction (Go st.step)

  SetInput s -> H.modify_ _ { input = s }

  Submit ev -> do
    liftEffect (B.preventDefault ev)
    st <- H.get
    let
      line = trim st.input
    H.modify_ _ { input = "" }
    unless (line == "") do
      log (In line)
      command line
    scrollConsole

  ChooseExample path -> do
    r <- H.liftAff (attempt (B.fetchText ("examples/" <> path)))
    case r of
      Right text -> do
        setSource text
        H.modify_ _ { step = 0, zoom = Nothing }
        handleAction (Go 0)
      Left _ -> flash ("Could not load " <> path)

  Open -> do
    { emitter, listener } <- liftEffect HS.create
    void (H.subscribe emitter)
    liftEffect (B.pickTextFile ".sc,.txt,text/plain" \name text -> HS.notify listener (Opened name text))

  Opened name text -> do
    setSource text
    H.modify_ _ { step = 0, zoom = Nothing }
    handleAction (Go 0)
    flash ("Opened " <> name)

  Save -> do
    st <- H.get
    liftEffect (B.downloadText (fileName st <> ".sc") "text/plain" st.source)

  ExportPdf -> withTrace \st t -> case st.script, st.proof of
    Just _, Just p | not (Array.null p.errors) -> do
      log (Err "The proof has errors; fix them before exporting.")
      scrollConsole
    _, _ -> do
      flash "Preparing the PDF… (the first export downloads the Typst compiler)"
      r <- H.liftAff (attempt (X.compilePdf (documentOf st t)))
      case r of
        Right bytes -> do
          liftEffect (X.downloadBytes (fileName st <> ".pdf") "application/pdf" bytes)
          flash "PDF ready."
        Left err -> do
          log (Err ("PDF export failed: " <> message err))
          flash "PDF export failed; see the console."
          scrollConsole

  ExportTypst -> withTrace \st t ->
    liftEffect (B.downloadText (fileName st <> ".typ") "text/plain" (documentOf st t))

  ExportLean -> withTrace \st t ->
    if t.parametric then case st.proof of
      Just p | Array.null p.errors -> case Universal.document p of
        Right doc -> liftEffect (B.downloadText (fileName st <> ".core.json") "application/json" doc)
        Left err -> do
          log (Err ("The Lean export failed: " <> err))
          scrollConsole
      _ -> do
        log (Err "The proof has errors; fix them before the Lean export.")
        scrollConsole
    else liftEffect (B.downloadText (fileName st <> ".core.json") "application/json" (Core.document t))

  Key ev -> do
    st <- H.get
    if st.helpOpen then
      when (B.keyOf ev == "Escape") (handleAction (ShowHelp false))
    else unless (B.isTypingTarget ev) do
      let
        n = stepCount st
        go a = liftEffect (B.preventDefault ev) *> handleAction a
      case B.keyOf ev of
        "ArrowLeft" -> go (Move (-1))
        "ArrowRight" -> go (Move 1)
        "Home" -> go (Go 0)
        "End" -> go (Go (n - 1))
        "+" -> go (ZoomBy 0.8)
        "=" -> go (ZoomBy 0.8)
        "-" -> go (ZoomBy 1.25)
        "0" -> go Fit
        _ -> pure unit

  Wheel ev -> do
    liftEffect (B.preventDefault ev)
    let
      w = B.wheelInfo ev
    zoomTo \v -> zoomAt (Number.exp (w.delta * 0.0015)) { fx: w.fx, fy: w.fy } v

  PointerDown ev -> do
    liftEffect (B.capturePointer ev)
    st <- H.get
    let
      p = B.pointerInfo ev
      grabbed = if Map.isEmpty st.pointers then grab st (atPointer st { fx: p.fx, fy: p.fy }) else Nothing
    H.modify_ \s -> s { pointers = Map.insert p.id { fx: p.fx, fy: p.fy } s.pointers, drag = grabbed }

  PointerMove ev -> do
    st <- H.get
    let
      p = B.pointerInfo ev
      here = { fx: p.fx, fy: p.fy }
    for_ (Map.lookup p.id st.pointers) \old -> case st.drag of
      Just d -> H.modify_ \s -> reinstance false s { override = Just (dragTo d (atPointer s here) (valuationOf s)), pointers = Map.insert p.id here s.pointers }
      Nothing -> do
        case Array.find (\(Tuple k _) -> k /= p.id) (Map.toUnfoldable st.pointers :: Array (Tuple Int _)) of
          -- one pointer: drag
          Nothing -> zoomTo (panBy { fx: here.fx - old.fx, fy: here.fy - old.fy })
          -- two pointers: pinch around their midpoint
          Just (Tuple _ other) -> do
            let
              d0 = distance old other
              d1 = distance here other
            when (d1 > 1.0e-6) do
              zoomTo (zoomAt (d0 / d1) { fx: (here.fx + other.fx) / 2.0, fy: (here.fy + other.fy) / 2.0 })
        H.modify_ \s -> s { pointers = Map.insert p.id here s.pointers }

  PointerUp ev -> do
    let
      p = B.pointerInfo ev
    H.modify_ \st -> st { pointers = Map.delete p.id st.pointers, drag = Nothing }

  ChooseCase v -> do
    H.modify_ \st -> reinstance true st { caseIx = fromMaybe 0 (Int.fromString v), override = Nothing, zoom = Nothing }
    st <- H.get
    handleAction (Go (stepCount st - 1))

  SetParam name v -> case Number.fromString v of
    Just x -> H.modify_ \st -> reinstance false st { override = Just (Map.insert name (SVNum (hundredths x)) (valuationOf st)) }
    Nothing -> pure unit

  -- a free point: an angle in degrees on a circle (t = tan(θ/2)), or t on a line
  SetFree name circle v -> case Number.fromString v of
    Just x ->
      let
        t = if circle then Number.tan (x * Number.pi / 360.0) else x
      in
        H.modify_ \st -> reinstance false st { override = Just (Map.insert name (SVNum (Q.fromInts (Int.round (t * 1000.0)) 1000)) (valuationOf st)) }
    Nothing -> pure unit

  ResetValues -> H.modify_ \st -> reinstance true st { override = Nothing, zoom = Nothing }

  ZoomBy f -> zoomTo (zoomAt f { fx: 0.5, fy: 0.5 })

  Fit -> H.modify_ _ { zoom = Nothing }

  ShowHelp open -> H.modify_ _ { helpOpen = open }

  HelpBackdrop ev -> when (B.isSelfTarget ev) (handleAction (ShowHelp false))
  where
  traverse_' :: forall a. (a -> H.HalogenM State Action () o m Unit) -> Maybe a -> H.HalogenM State Action () o m Unit
  traverse_' f = maybe (pure unit) f

  either' :: forall e a b. b -> (a -> b) -> Either e a -> b
  either' d f = case _ of
    Right a -> f a
    Left _ -> d
  distance a b = Number.sqrt ((a.fx - b.fx) * (a.fx - b.fx) + (a.fy - b.fy) * (a.fy - b.fy))

-- | The Typst document: the parametric one for a script with variables.
documentOf :: State -> Trace -> String
documentOf st t = case st.script, st.proof of
  Just script, Just p -> TypstParam.document script p
  _, _ -> Typst.document t

-- | Changes the view, keeping its size within reasonable bounds of the fit.
zoomTo :: forall o m. (Viewport -> Viewport) -> H.HalogenM State Action () o m Unit
zoomTo f = H.modify_ \st ->
  let
    v = f (effectiveView st)
    size = v.maxX - v.minX
    fit = st.view.maxX - st.view.minX
  in
    if size < fit * 1.0e-4 || size > fit * 100.0 then st else st { zoom = Just v }

-- | Runs an export when the script has no errors.
withTrace
  :: forall o m
   . MonadAff m
  => (State -> Trace -> H.HalogenM State Action () o m Unit)
  -> H.HalogenM State Action () o m Unit
withTrace f = do
  st <- H.get
  case st.parseError, st.trace of
    Nothing, Just t | isNothing t.error -> f st t
    _, _ -> flash "Fix the errors in the script before exporting."

fileName :: State -> String
fileName st = case st.trace >>= _.title of
  Just t -> String.toLower (joinWith "-" (Array.filter (_ /= "") (String.split (Pattern " ") (onlyWordChars t))))
  Nothing -> "construction"

foreign import onlyWordChars :: String -> String

-- | The saved draft, or else the first example.
initialSource :: forall o m. MonadAff m => Array B.Example -> H.HalogenM State Action () o m String
initialSource examples = do
  draft <- liftEffect (B.storageGet storageKey)
  case draft of
    Just src | trim src /= "" -> pure src
    _ -> case Array.head examples of
      Nothing -> pure fallback
      Just e -> do
        r <- H.liftAff (attempt (B.fetchText ("examples/" <> e.path)))
        pure case r of
          Right text -> text
          Left _ -> fallback
  where
  fallback = "given A = (0, 0)\ngiven B = (1, 0)\n"

setSource :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
setSource src = do
  H.modify_ (recompute src)
  st <- H.get
  for_ st.editor \view -> liftEffect (Editor.setValue view src)
  liftEffect (B.storageSet storageKey src)
  syncEditor false

syncEditor :: forall o m. MonadAff m => Boolean -> H.HalogenM State Action () o m Unit
syncEditor scroll = do
  st <- H.get
  for_ st.editor \view ->
    liftEffect (Editor.markLines view (maybe 0 _.line (currentStep st)) (errorLine st) scroll)

-- | Shows a short status message for a few seconds.
flash :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
flash msg = do
  H.modify_ _ { status = Just msg }
  void $ H.fork do
    H.liftAff (delay (Milliseconds 2500.0))
    H.modify_ \st -> if st.status == Just msg then st { status = Nothing } else st

log :: forall o m. Entry -> H.HalogenM State Action () o m Unit
log e = H.modify_ \st -> st { console = Array.snoc st.console e }

scrollConsole :: forall o m. MonadAff m => H.HalogenM State Action () o m Unit
scrollConsole = H.getHTMLElementRef consoleRef >>= maybe (pure unit) (liftEffect <<< B.scrollToBottom)

-------------------------------------------------------------------------------
-- The console

helpText :: Array String
helpText =
  [ "Statements are added to the script, e.g.  C = meet c1 c2 left A B"
  , "show X     the exact value of X"
  , "goto N     jump to step N"
  , "undo       remove the last line of the script"
  , "steps      list the steps"
  , "clear      clear the console"
  , "export pdf | typst | lean   download the proof"
  , "The Help button (top right) explains the language."
  ]

command :: forall o m. MonadAff m => String -> H.HalogenM State Action () o m Unit
command line = case words line of
  [ "help" ] -> for_ helpText (log <<< Out)
  [ "clear" ] -> H.modify_ _ { console = [] }
  [ "export", "pdf" ] -> handleAction ExportPdf
  [ "export", "typst" ] -> handleAction ExportTypst
  [ "export", "lean" ] -> handleAction ExportLean
  [ "undo" ] -> do
    st <- H.get
    let
      ls = String.split (Pattern "\n") st.source
      kept = dropLastStatement ls
    setSource (joinWith "\n" kept)
    log (Out "Removed the last line.")
  [ "steps" ] -> do
    st <- H.get
    for_ (maybe [] _.steps st.trace) \s -> log (Out (show (s.index + 1) <> ". " <> s.text))
  [ "goto", n ] -> case Int.fromString n of
    Just k -> handleAction (Go (k - 1)) *> log (Out ("Step " <> show k))
    Nothing -> log (Err "goto expects a step number")
  [ "show", name ] -> do
    st <- H.get
    case st.trace >>= \t -> Map.lookup name t.scope >>= \i -> Map.lookup i t.objects of
      Just info -> log (Out (name <> " = " <> describeObj info.obj))
      Nothing -> log (Err (name <> " is not defined"))
  _ -> case parseStatement line of
    Left e -> log (Err ("column " <> show e.column <> ": " <> e.message))
    Right _ -> do
      st <- H.get
      let
        base = if st.source == "" || CU.takeRight 1 st.source == "\n" then st.source else st.source <> "\n"
        candidate = base <> line <> "\n"
        newLine = Array.length (String.split (Pattern "\n") base)
        next = recompute candidate st
      case next.parseError, next.trace >>= _.error of
        Just e, _ -> log (Err e.message)
        _, Just e | e.line == newLine -> log (Err e.message)
        _, _ -> do
          setSource candidate
          handleAction (Go (stepCount next - 1))
          st' <- H.get
          for_ (currentStep st') \s -> describeStep st' s
  where
  words s = Array.filter (_ /= "") (String.split (Pattern " ") s)
  describeStep st s = case st.trace of
    Nothing -> pure unit
    Just t -> do
      for_ s.objects \i -> for_ (Map.lookup i t.objects) \o -> log (Out (o.name <> " = " <> describeObj o.obj))
      for_ s.verdicts \v -> log
        ( if v.holds then Out ("✓ " <> v.text <> details v.detail)
          else Err ("✗ " <> v.text <> details v.detail)
        )
  details d = if Array.null d then "" else "  (" <> detailText d <> ")"

-- | Drops the last non-blank line.
dropLastStatement :: Array String -> Array String
dropLastStatement ls = case Array.findLastIndex (\l -> trim l /= "") ls of
  Just i -> Array.take i ls
  Nothing -> ls

-------------------------------------------------------------------------------
-- Rendering

cls :: forall r i. String -> HP.IProp (class :: String | r) i
cls = HP.class_ <<< H.ClassName

-- | The logo: the favicon's circle and straight line.
logo :: forall w i. HH.HTML w i
logo =
  svg "svg" [ at "class" "logo", at "viewBox" "0 0 32 32", at "aria-hidden" "true" ]
    [ svg "circle" [ at "class" "logo-circle", at "cx" "16", at "cy" "16", at "r" "11" ] []
    , svg "line" [ at "class" "logo-line", at "x1" "3", at "y1" "27", at "x2" "29", at "y2" "5" ] []
    ]
  where
  svg name = HH.elementNS (Namespace "http://www.w3.org/2000/svg") (ElemName name)
  at k v = HP.attr (AttrName k) v

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  HH.div [ cls "app" ]
    ( [ HH.header [ cls "bar" ]
          [ HH.div [ cls "brand" ] [ logo, HH.span_ [ HH.text "Straightedge & Compass" ] ]
          , HH.div [ cls "actions" ]
              [ HH.select
                  [ cls "control"
                  , HP.title "Load an example"
                  , HE.onValueChange \v -> ChooseExample v
                  ]
                  ( [ HH.option [ HP.value "", HP.selected true, HP.disabled true ] [ HH.text "Examples…" ] ]
                      <> map (\e -> HH.option [ HP.value e.path ] [ HH.text e.title ]) st.examples
                  )
              , button "Open" "Open a .sc script" Open
              , button "Save" "Download the script" Save
              , button "PDF" "Export the proof as a PDF, one page per snapshot" ExportPdf
              , button "Lean" "Export the core JSON for the Lean elaborator" ExportLean
              , button "Help" "The script language and the controls" (ShowHelp true)
              ]
          ]
      , HH.main [ cls "workspace" ]
          [ HH.section [ cls "pane pane-script" ]
              [ HH.div [ cls "pane-head" ]
                  [ HH.h2_ [ HH.text "Script" ], problem st ]
              , HH.div [ cls "editor", HP.ref editorRef ] []
              ]
          , HH.section [ cls "pane pane-figure" ]
              [ HH.div [ cls "pane-head" ]
                  [ HH.h2_ [ HH.text (fromMaybe "Construction" (st.trace >>= _.title)) ] ]
              , HH.div
                  [ cls "figure"
                  , HE.handler (EventType "wheel") Wheel
                  , HE.handler (EventType "pointerdown") PointerDown
                  , HE.handler (EventType "pointermove") PointerMove
                  , HE.handler (EventType "pointerup") PointerUp
                  , HE.handler (EventType "pointercancel") PointerUp
                  , HE.handler (EventType "dblclick") (const Fit)
                  ]
                  [ maybe (HH.text "") (\t -> figureView (figure t (effectiveView st) (range st))) st.trace ]
              , controls st
              , paramPanel st
              , stepPanel st
              ]
          ]
      , HH.section [ cls "console-pane" ]
          [ HH.h2_ [ HH.text "Console" ]
          , consoleView st
          ]
      , case st.status of
          Just s -> HH.div [ cls "toast", HP.attr (AttrName "role") "status" ] [ HH.text s ]
          Nothing -> HH.text ""
      ]
        <> (if st.helpOpen then [ helpView (ShowHelp false) HelpBackdrop ] else [])
    )
  where
  button label title action = HH.button [ cls "control", HP.title title, HE.onClick \_ -> action ] [ HH.text label ]

problem :: forall w i. State -> HH.HTML w i
problem st = case st.parseError, st.proof of
  Just e, _ -> HH.span [ cls "badge bad" ] [ HH.text ("Line " <> show e.line <> ":" <> show e.column <> " — " <> e.message) ]
  _, Just p -> case Array.head p.errors of
    Just e -> HH.span [ cls "badge bad" ] [ HH.text ("Line " <> show e.line <> " — " <> e.message) ]
    Nothing ->
      let
        n = Array.length (caseList p)
      in
        HH.span [ cls "badge good" ] [ HH.text ("✓ proved for all values" <> (if n > 1 then " (" <> show n <> " cases)" else "")) ]
  _, _ -> problemInstance st

problemInstance :: forall w i. State -> HH.HTML w i
problemInstance st = case st.parseError, st.trace >>= _.error of
  Just e, _ -> HH.span [ cls "badge bad" ] [ HH.text ("Line " <> show e.line <> ":" <> show e.column <> " — " <> e.message) ]
  _, Just e -> HH.span [ cls "badge bad" ]
    [ HH.text ("Line " <> show e.line <> " — " <> e.message <> context e.context) ]
  _, _ -> case st.trace of
    Just t | Array.length t.steps > 0 -> HH.span [ cls "badge good" ] [ HH.text ("✓ " <> show (Array.length t.asserts) <> " checks hold") ]
    _ -> HH.text ""
  where
  context cs = if Array.null cs then "" else " (" <> joinWith ", " cs <> ")"

consoleView :: forall m. State -> H.ComponentHTML Action () m
consoleView st =
  HH.div [ cls "console" ]
    [ HH.div [ cls "console-log", HP.ref consoleRef, HP.attr (AttrName "aria-live") "polite" ]
        (map entry st.console)
    , HH.form [ cls "console-form", HE.onSubmit Submit ]
        [ HH.span [ cls "prompt" ] [ HH.text ">" ]
        , HH.input
            [ cls "console-input"
            , HP.value st.input
            , HP.placeholder "a statement, or a command such as show P1"
            , HP.attr (AttrName "aria-label") "Console input"
            , HP.autocomplete HP.AutocompleteOff
            , HP.spellcheck false
            , HE.onValueInput SetInput
            ]
        ]
    ]
  where
  entry = case _ of
    In s -> HH.div [ cls "in" ] [ HH.text ("> " <> s) ]
    Out s -> HH.div [ cls "out" ] [ HH.text s ]
    Err s -> HH.div [ cls "err" ] [ HH.text s ]

controls :: forall m. State -> H.ComponentHTML Action () m
controls st =
  let
    n = stepCount st
    ps = pagesOf st
    page = currentPage st
    atStart = if st.byPage then maybe true (\p -> p.index == 0) page else st.step <= 0
    atEnd = if st.byPage then maybe true (\p -> p.index >= Array.length ps - 1) page else st.step >= n - 1
    expandable = not st.byPage && maybe false (isJust <<< _.macro) (currentStep st)
    position = case st.byPage, page of
      _, _ | n == 0 -> "no steps"
      true, Just p ->
        "page " <> show (p.index + 1) <> " / " <> show (Array.length ps) <> " · steps " <> show (p.page.first + 1)
          <> "–"
          <> show (p.page.last + 1)
      _, _ -> "step " <> show (st.step + 1) <> " / " <> show n
    unit' = if st.byPage then "page" else "step"
  in
    HH.div [ cls "controls" ]
      [ ctrl "⏮" ("First " <> unit' <> " (Home)") (Go 0) atStart
      , ctrl "◀" ("Previous " <> unit' <> " (←)") (Move (-1)) atStart
      , HH.input
          [ cls "slider"
          , HP.type_ HP.InputRange
          , HP.min 0.0
          , HP.max (Int.toNumber (max 0 (n - 1)))
          , HP.value (show st.step)
          , HP.attr (AttrName "list") "snapshot-ticks"
          , HP.attr (AttrName "aria-label") "Step"
          , HE.onValueInput \v -> Go (fromMaybe 0 (Int.fromString v))
          ]
      , HH.datalist [ HP.id "snapshot-ticks" ]
          (map (\p -> HH.option [ HP.value (show p.last) ] []) ps)
      , ctrl "▶" ("Next " <> unit' <> " (→)") (Move 1) atEnd
      , ctrl "⏭" ("Last " <> unit' <> " (End)") (Go (n - 1)) atEnd
      , HH.span [ cls "step-count" ] [ HH.text position ]
      , HH.span [ cls "zoom" ]
          [ ctrl "−" "Zoom out (−, wheel)" (ZoomBy 1.25) false
          , ctrl "+" "Zoom in (+, wheel)" (ZoomBy 0.8) false
          , HH.button [ cls "control", HP.title "Fit the figure (0, double-click)", HE.onClick \_ -> Fit ] [ HH.text "Fit" ]
          ]
      , HH.label [ cls "toggle" ]
          [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked st.byPage, HE.onChange \_ -> ToggleByPage ]
          , HH.text " by page"
          ]
      , HH.label [ cls ("toggle" <> if expandable then "" else " muted") ]
          [ HH.input [ HP.type_ HP.InputCheckbox, HP.checked st.expanded, HP.disabled st.byPage, HE.onChange \_ -> ToggleExpanded ]
          , HH.text " macro steps"
          ]
      ]
  where
  ctrl label title action disabled =
    HH.button [ cls "control icon", HP.title title, HP.disabled disabled, HE.onClick \_ -> action ] [ HH.text label ]

-- | A number with at most two decimals, without a trailing ".0".
number :: Number -> String
number x =
  let
    r = Number.round (x * 100.0) / 100.0
    i = Int.round r
  in
    String.replaceAll (Pattern "-") (String.Replacement "−")
      (if Int.toNumber i == r then show i else show r)

-- | The variables of a universal statement: the case shown, where the
-- | current configuration lies, and sliders for the values.
paramPanel :: forall m. State -> H.ComponentHTML Action () m
paramPanel st = case st.proof, st.trace of
  Just p, Just t ->
    let
      cs = caseList p
      PNode root = p.root
      vals = valuationOf st
      params = Array.filter (\v -> isNothing v.free) root.vars
      frees = Array.mapMaybe (\v -> (\f -> { name: v.name, object: f.object, circle: f.frame.kind == "circle" }) <$> v.free) root.vars
      here = case t.outside, t.error of
        Just o, _ -> "outside the theorem: " <> o.message
        _, Just e -> "outside the theorem: " <> e.message
        _, _ ->
          if Array.null t.path then "this configuration satisfies the hypotheses"
          else "this configuration is in the case " <> joinWith " › " (map (labelText <<< _.label) t.path)
      slider v =
        let
          x = case Map.lookup v.name vals of
            Just (SVNum q) -> Q.toNumber q
            _ -> 0.0
          lo = min (-3.0) (x - 1.0)
          hi = max 3.0 (x + 1.0)
        in
          HH.label [ cls "param" ]
            [ HH.span [ cls "param-name" ] [ HH.text (v.name <> " = " <> number x) ]
            , HH.input
                [ cls "slider"
                , HP.type_ HP.InputRange
                , HP.min lo
                , HP.max hi
                , HP.step (HP.Step 0.01)
                , HP.value (show x)
                , HP.attr (AttrName "aria-label") v.name
                , HE.onValueInput (SetParam v.name)
                ]
            ]
      -- a free point: its angle from the circle's through-point, or t on a line
      freeSlider f =
        let
          tNow = case Map.lookup f.name t.valuation of
            Just (SVNum q) -> Q.toNumber q
            _ -> 0.0
          x = if f.circle then 2.0 * Number.atan tNow * 180.0 / Number.pi else tNow
          lo = if f.circle then -179.0 else min (-2.0) (x - 1.0)
          hi = if f.circle then 179.0 else max 3.0 (x + 1.0)
        in
          HH.label [ cls "param" ]
            [ HH.span [ cls "param-name" ]
                [ HH.text (f.name <> " on " <> f.object <> ": " <> (if f.circle then number (Number.round x) <> "°" else "t = " <> number x)) ]
            , HH.input
                [ cls "slider"
                , HP.type_ HP.InputRange
                , HP.min lo
                , HP.max hi
                , HP.step (HP.Step (if f.circle then 1.0 else 0.01))
                , HP.value (show x)
                , HP.attr (AttrName "aria-label") ("position of " <> f.name <> " on " <> f.object)
                , HE.onValueInput (SetFree f.name f.circle)
                ]
            ]
      draggable = map _.name (maybe [] draggables st.script)
    in
      HH.div [ cls "params" ]
        ( [ HH.div [ cls "params-head" ]
              ( ( if Array.length cs > 1 then
                    [ HH.label [ cls "case-pick" ]
                        [ HH.text "Case "
                        , HH.select [ cls "control", HE.onValueChange ChooseCase ]
                            (Array.mapWithIndex (\i c -> HH.option [ HP.value (show i), HP.selected (i == st.caseIx) ] [ HH.text c.label ]) cs)
                        ]
                    ]
                  else []
                )
                  <>
                    [ HH.span [ cls ("case-now" <> if isJust t.outside || isJust t.error then " bad" else "") ] [ HH.text here ]
                    , HH.button [ cls "control", HP.title "Back to the samples of the script", HP.disabled (isNothing st.override), HE.onClick \_ -> ResetValues ] [ HH.text "Reset" ]
                    ]
              )
          ]
            <> map slider params
            <> map freeSlider frees
            <>
              ( if Array.null draggable then []
                else [ HH.p [ cls "muted params-hint" ] [ HH.text ("Use the sliders, or drag " <> joinWith ", " draggable <> " in the figure.") ] ]
              )
        )
  _, _ -> HH.text ""

stepPanel :: forall w i. State -> HH.HTML w i
stepPanel st = case st.trace of
  Just t | stepCount st > 0 ->
    let
      r = range st
      colours = newColours t r
      steps = Array.mapMaybe (Array.index t.steps) (r.newFrom .. r.upTo)
    in
      HH.div [ cls "step" ]
        ( (if r.newFrom == 0 then map note t.intro else [])
            <> Array.concatMap (stepBlock t colours) steps
            <> [ generality st ]
        )
  _ -> HH.div [ cls "step" ] [ HH.p [ cls "muted" ] [ HH.text "Nothing to show yet." ] ]
  where
  stepBlock t colours s =
    [ HH.div [ cls "step-head" ]
        [ HH.span [ cls "step-kind" ] [ HH.text (show (s.index + 1) <> " · " <> kindLabel s.kind) ]
        , HH.code_ [ HH.text s.text ]
        ]
    ]
      <> map note s.notes
      <> objects t colours s
      <> proofLines s
      <> map verdict s.verdicts
      <> (if not st.byPage && st.expanded && not (Array.null s.substeps) then [ substeps s ] else [])
  kindLabel = case _ of
    SGiven -> "given"
    SConstruct -> "construct"
    SAssert -> "check"
    SAssume -> "assume"
    SCase -> "case"
  note n = HH.p [ cls "note" ] [ HH.text n ]
  -- for a universal statement: the hypotheses a step introduces and why
  -- its claims hold for all values
  proofLines s = case proofSteps of
    Nothing -> []
    Just ps -> case Array.find (\x -> x.index == s.index) ps.steps of
      Nothing -> []
      Just ps' ->
        map (\h -> HH.div [ cls "hyp" ] [ HH.text (hypKind h.origin <> ": " <> hypDisplay ps.names h) ]) ps'.hyps
          <> map (\v -> HH.div [ cls (if v.holds then "verdict good" else "verdict bad") ] [ HH.text ((if v.holds then "✓ for all values: " else "✗ ") <> v.text <> " — " <> v.why) ]) (Array.filter (\_ -> s.kind == SAssert) ps'.verdicts)
  proofSteps = do
    p <- st.proof
    t <- st.trace
    let
      along = stepsAlong p.root (map _.label t.path)
      PNode leaf = along.node
    pure { steps: along.steps :: Array PStep, names: namesOf leaf.vars }
  hypKind = case _ of
    Computed -> "needs"
    Assumed -> "assumes"
    BranchCond -> "case"
  objects t colours s =
    Array.mapMaybe
      ( \i -> Map.lookup i t.objects <#> \o ->
          HH.div [ cls "obj" ]
            [ HH.span [ cls ("obj-name" <> maybe "" (\k -> " c" <> show k) (Map.lookup i colours)) ] [ HH.text o.name ]
            , HH.text (" = " <> describeObj o.obj)
            ]
      )
      s.objects
  verdict v =
    HH.div [ cls (if v.holds then "verdict good" else "verdict bad") ]
      [ HH.text
          ( (if v.holds then "✓ " else "✗ ") <> v.text
              <> (if Array.null v.detail then "" else "  (" <> detailText v.detail <> ")")
              <> if Array.null v.context then "" else "  (in " <> joinWith ", " v.context <> ")"
          )
      ]
  substeps s =
    HH.ol [ cls "substeps" ]
      ( map
          ( \sub -> HH.li [ HP.style ("margin-left: " <> show (sub.depth - 1) <> "em") ]
              [ HH.code_ [ HH.text sub.text ] ]
          )
          s.substeps
      )

generality :: forall w i. State -> HH.HTML w i
generality st = case st.proof, st.trace of
  Just p, Just t ->
    let
      along = stepsAlong p.root (map _.label t.path)
      PNode leaf = along.node
      names = namesOf leaf.vars
    in
      HH.div [ cls "generality" ]
        ( [ HH.text ("Universal statement: the claims hold for all values of the variables" <> (if Array.null leaf.theorem then "." else " such that")) ]
            <> [ HH.ul_ (map (\h -> HH.li_ [ HH.text (hypDisplay names h) ]) leaf.theorem) ]
        )
  _, _ -> generalityFixed st

generalityFixed :: forall w i. State -> HH.HTML w i
generalityFixed st = case st.trace of
  Just t | Array.length t.steps > 0 ->
    HH.p [ cls "generality" ]
      [ HH.text
          if isGeneral t then
            show t.givens <> " given point" <> (if t.givens == 1 then "" else "s")
              <> ": constructions commute with similarities, so these exact checks prove the statement for every choice of the given points."
          else
            show t.givens <> " given points: the exact checks prove the statement for this configuration only."
      ]
  _ -> HH.text ""
