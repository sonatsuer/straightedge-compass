-- | PDF compilation in the browser (typst.ts, loaded on demand).
module App.Export
  ( Bytes
  , compilePdf
  , downloadBytes
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Bytes :: Type

foreign import compilePdfImpl :: String -> EffectFnAff Bytes

-- | Compiles a Typst document to PDF bytes.
compilePdf :: String -> Aff Bytes
compilePdf = fromEffectFnAff <<< compilePdfImpl

foreign import downloadBytes :: String -> String -> Bytes -> Effect Unit
