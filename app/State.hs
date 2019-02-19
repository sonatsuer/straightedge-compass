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

asPoint :: Input a -> CommandM Point
asPoint inp = do
  MapObject rawObj <- inputToMapObject inp
  case rawObj of
    RawPoint p ->
      return p
    RawLine l ->
      throwE $ "Expecting a point but got a line."
    RawCircle _ ->
      throwE $ "Expecting a point but got a circle."


asLine :: Input a -> CommandM Line
asLine inp = do
  MapObject rawObj <- inputToMapObject inp
  case rawObj of
    RawPoint _ ->
      throwE $ "Expecting a line but got a point."
    RawLine l ->
      return l
    RawCircle _ ->
      throwE $ "Expecting a line but got a circle."


asCircle :: Input a -> CommandM Circle
asCircle inp = do
  MapObject rawObj <- inputToMapObject inp
  case rawObj of
    RawPoint _ ->
      throwE $ "Expecting a circle but got a point."
    RawLine _ ->
      throwE $ "Expecting a circle but got a line."
    RawCircle c ->
      return c
