{-# LANGUAGE
    LambdaCase
  , GADTs
  , RankNTypes
#-}

module State where

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class (lift)
import           Command
import           Geometry
import qualified Data.Map.Strict as M

data MapObject where
  MapObject :: forall a . RawObject a -> MapObject

type GlobalState = M.Map String MapObject
type CommandM = ExceptT String (State GlobalState)

emptyState :: GlobalState
emptyState = M.empty

issue :: Command -> CommandM String
issue = \case
  Construct construction ->
    show . snd <$> evalConstruction construction
  NameResult construction capture -> do
    result <- evalConstruction construction
    case (capture, result) of
      (_, _) ->
        undefined
  NameObject obj name ->
    undefined
  Discard name ->
    undefined
  Show name ->
    undefined

evalConstruction :: Construction a -> CommandM (a, String)
evalConstruction = undefined

inputToMapObject :: Input a -> CommandM MapObject
inputToMapObject = \case
  Raw rawObj ->
    return $ MapObject rawObj
  Reference name ->
    (lift $ gets $ M.lookup name) >>= \case
      Nothing ->
        throwE $ "Object with name " ++ name ++ "does not exist"
      Just mapObj ->
        return mapObj
