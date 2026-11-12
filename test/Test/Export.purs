module Test.Export (spec) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String as String
import Effect.Class (liftEffect)
import Export.Core as Core
import Export.Typst as Typst
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafeCrashWith)
import Proof.Eval (Trace, evaluateSource)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

trace :: String -> Trace
trace src = case evaluateSource src of
  Right t -> t
  Left e -> unsafeCrashWith e.message

count :: String -> String -> Int
count needle hay = Array.length (split (Pattern needle) hay) - 1

field :: String -> Json -> Maybe Json
field k j = toObject j >>= Object.lookup k

spec :: Spec Unit
spec = describe "Exports" do
  describe "Typst" do
    it "one page per snapshot, plus the final piece" do
      src <- liftEffect (readTextFile UTF8 "examples/euclid-i-1/euclid-i-1.sc")
      let
        doc = Typst.document (trace src)
      -- one snapshot: two pages, so one page break
      count "#pagebreak()" doc `shouldEqual` 1
    it "consecutive snapshots count once, a trailing one adds no page" do
      let
        doc = Typst.document
          (trace "given A = (0, 0)\nsnapshot\nsnapshot\ngiven B = (1, 0)\nsnapshot")
      count "#pagebreak()" doc `shouldEqual` 1
    it "the title is text, notes are Typst markup" do
      let
        doc = Typst.document (trace "title \"#x\"\ngiven A = (0, 0)\nnote \"$x^2$ and \\*\"")
      String.contains (Pattern "#\"#x\"") doc `shouldEqual` true
      String.contains (Pattern "[$x^2$ and \\*]") doc `shouldEqual` true
    it "uses Typst radicals, not the square root sign" do
      src <- liftEffect (readTextFile UTF8 "examples/euclid-i-1/euclid-i-1.sc")
      let
        doc = Typst.document (trace src)
      String.contains (Pattern "sqrt(3)") doc `shouldEqual` true
      String.contains (Pattern "√") doc `shouldEqual` false

  describe "core" do
    it "describes Euclid I.1" do
      src <- liftEffect (readTextFile UTF8 "examples/euclid-i-1/euclid-i-1.sc")
      case jsonParser (Core.document (trace src)) of
        Left e -> fail e
        Right j -> do
          (field "format" j >>= toString) `shouldEqual` Just "straightedge-compass-core"
          (field "version" j >>= toNumber) `shouldEqual` Just 1.0
          (Array.length <$> (field "tower" j >>= toArray)) `shouldEqual` Just 1
          (Array.length <$> (field "objects" j >>= toArray)) `shouldEqual` Just 5
          (Array.length <$> (field "asserts" j >>= toArray)) `shouldEqual` Just 2
    it "the committed example exports are up to date" do
      for' [ "euclid-i-1", "heptadecagon", "regular-pentagon", "regular-hexagon", "regular-15-gon", "square-on-segment" ] \name -> do
        src <- liftEffect (readTextFile UTF8 ("examples/" <> name <> "/" <> name <> ".sc"))
        committed <- liftEffect (readTextFile UTF8 ("examples/" <> name <> "/" <> name <> ".core.json"))
        (Core.document (trace src) <> "\n" == committed) `shouldEqual` true
  where
  for' xs f = void (Array.foldM (\_ x -> f x) unit xs)
