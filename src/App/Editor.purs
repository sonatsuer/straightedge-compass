-- | The script editor (CodeMirror 6, through FFI).
module App.Editor
  ( EditorView
  , create
  , getValue
  , setValue
  , markLines
  , tokensOf
  ) where

import Prelude

import Effect (Effect)
import Web.HTML (HTMLElement)

foreign import data EditorView :: Type

foreign import createImpl :: HTMLElement -> String -> (String -> Effect Unit) -> Effect EditorView

-- | Creates an editor in the element. The callback receives the text after
-- | edits (debounced).
create :: HTMLElement -> String -> (String -> Effect Unit) -> Effect EditorView
create = createImpl

-- | The tokens of a snippet and their kinds (`keyword`, `comment`, `string`,
-- | `number`, `macroName`, `operator`, `variableName`, or empty), as the
-- | editor highlights them.
foreign import tokensOf :: String -> Array { text :: String, kind :: String }

foreign import getValue :: EditorView -> Effect String
foreign import setValue :: EditorView -> String -> Effect Unit

-- | `markLines view current error scroll` highlights the current step's line
-- | and the error line (0 for none), scrolling the current line into view.
foreign import markLines :: EditorView -> Int -> Int -> Boolean -> Effect Unit
