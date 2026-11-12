-- | Small browser services for the app (all foreign).
module App.Browser
  ( Example
  , fetchText
  , downloadText
  , pickTextFile
  , parseExamples
  , storageGet
  , storageSet
  , isTypingTarget
  , isSelfTarget
  , preventDefault
  , keyOf
  , scrollToBottom
  , pointerInfo
  , wheelInfo
  , capturePointer
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.Event.Event (Event)
import Web.HTML (HTMLElement)

-- | An entry of `examples/index.json`.
type Example = { path :: String, title :: String }

foreign import fetchTextImpl :: String -> EffectFnAff String

fetchText :: String -> Aff String
fetchText = fromEffectFnAff <<< fetchTextImpl

foreign import downloadText :: String -> String -> String -> Effect Unit
foreign import pickTextFile :: String -> (String -> String -> Effect Unit) -> Effect Unit
foreign import parseExamples :: String -> Array Example
foreign import storageGetImpl :: String -> Effect (Nullable String)
foreign import storageSet :: String -> String -> Effect Unit
foreign import isTypingTarget :: Event -> Boolean
foreign import isSelfTarget :: Event -> Boolean
foreign import preventDefault :: Event -> Effect Unit
foreign import keyOf :: Event -> String
foreign import scrollToBottom :: HTMLElement -> Effect Unit

-- | A pointer event: its pointer id and position as fractions of the box of
-- | the element that handles it.
foreign import pointerInfo :: Event -> { id :: Int, fx :: Number, fy :: Number }

-- | A wheel event: vertical delta in pixels and position as fractions.
foreign import wheelInfo :: Event -> { delta :: Number, fx :: Number, fy :: Number }

-- | Keeps sending a pointer's events to the element while it is pressed.
foreign import capturePointer :: Event -> Effect Unit

storageGet :: String -> Effect (Maybe String)
storageGet key = toMaybe <$> storageGetImpl key
