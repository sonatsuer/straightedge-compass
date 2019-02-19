{-# LANGUAGE
    LambdaCase
  , GADTs
  , RankNTypes
#-}

module State where

import           Control.Monad (when)
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
    (result, msg) <- evalConstruction construction
    assignResult capture (MapObject <$> result)
  NameObject obj name -> do
    assignObject name (MapObject obj)
  Discard name ->
    discard name
  Show name ->
    display name

evalConstruction :: Construction a -> CommandM (Result (RawObject a), String)
evalConstruction = undefined

inputToMapObject :: Input a -> CommandM MapObject
inputToMapObject = \case
  Raw rawObj ->
    return $ MapObject rawObj
  Reference name ->
    getMapObject name

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

assign :: Name -> MapObject -> CommandM String
assign name mapObj@(MapObject rawObj) = do
  lift $ modify (M.insert name mapObj)
  return $ "Now the name " ++ name ++ "is assigned to\n" ++ show rawObj

isFree :: Name -> CommandM Bool
isFree name = lift $ gets $ M.member name

getMapObject :: Name -> CommandM MapObject
getMapObject name =
  (lift $ gets $ M.lookup name) >>= \case
    Nothing ->
      throwE $ "Object with name " ++ name ++ "does not exist"
    Just mapObj ->
      return mapObj

checkFree :: Name -> CommandM ()
checkFree name = do
  free <- isFree name
  when (not free) (throwE $ name ++ " is not free.")

checkOccupied :: Name -> CommandM ()
checkOccupied name = do
  free <- isFree name
  when free (throwE $ name ++ " is not being used.")

assignResult :: Capture -> Result MapObject -> CommandM String
assignResult capture result =
  case (capture, result) of
    (_, None) ->
      throwE "No value to capture."
    (_, Infinite) ->
      throwE "Infinitely many values to capture."
    (Single name, Unique obj) ->
      checkFree name >> assign name obj
    (Single _, Distinct _ _) ->
      throwE "Cannot assign a single name to two values."
    (Pair name1 name2, Distinct obj1 obj2) -> do
      when (name1 == name2) (throwE "Names should be distint")
      checkFree name1
      checkFree name2
      msg1 <- assign name1 obj1
      msg2 <- assign name2 obj2
      return $ msg1 ++ "\n" ++ msg2
    (Pair _ _, Unique _) ->
      throwE "Will not assign two names to a single value."

assignObject :: Name -> MapObject -> CommandM String
assignObject name obj =
  checkFree name >> assign name obj

discard :: Name -> CommandM String
discard name = do
  free <- isFree name
  when free (throwE $ name ++ " is not being used.")
  lift $ modify $ M.delete name
  return $ "Object named " ++ name ++ " is discarded."

display :: Name -> CommandM String
display name = do
  MapObject rawObj <- getMapObject name
  return $ show rawObj