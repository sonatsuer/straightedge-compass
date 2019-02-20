{-# LANGUAGE
    LambdaCase
  , GADTs
  , ExplicitForAll
  , GeneralizedNewtypeDeriving
#-}

module State where

import           Control.Monad (when)
import           Control.Monad.Except (ExceptT, MonadError, throwError)
import           Control.Monad.State.Strict (State, MonadState, gets, modify)
import qualified Data.Map.Strict as M
-------------------------------------------------------------------------------
import           Command
import           Geometry

data MapObject where
  MapObject :: forall a . RawObject a -> MapObject

type GlobalState = M.Map String MapObject
newtype CommandM a =
  CommandM { _runCommandM :: ExceptT String (State GlobalState) a }
  deriving (Functor, Applicative, Monad, MonadState GlobalState, MonadError String)

emptyState :: GlobalState
emptyState = M.empty

issue :: Command -> CommandM String
issue = \case
  Construct construction ->
    show <$> evalConstruction construction
  NameResult construction capture -> do
    result <- evalConstruction construction
    assignResult capture (MapObject <$> result)
  NameObject obj name -> do
    assignObject name (MapObject obj)
  Discard name ->
    discardName name
  Show name ->
    displayName name


evalConstruction
  :: Construction (Result a)
  -> CommandM (Result (RawObject a))
evalConstruction = \case
  LineLineIntersection inp1 inp2 -> do
    l1 <- asLine inp1
    l2 <- asLine inp2
    return $ RawPoint <$> intersectionOfLines l1 l2
  LineCircleIntersection inp1 inp2 -> do
    l <- asLine inp1
    c <- asCircle inp2
    return $ RawPoint <$> intersectionOfLineAndCircle l c
  CircleCircleIntersection inp1 inp2 -> do
    c1 <- asCircle inp1
    c2 <- asCircle inp2
    return $ RawPoint <$> intersectionOfCircles c1 c2
  LineThroughPoints inp1 inp2 -> do
    p1 <- asPoint inp1
    p2 <- asPoint inp2
    return $ RawLine <$> lineThroughPoints p1 p2
  CircleFromPoints inp1 inp2 -> do
    p1 <- asPoint inp1
    p2 <- asPoint inp2
    return $ RawCircle <$> circleFromCenterAndPoint p1 p2

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
    RawLine _ ->
      throwError $ "Expecting a point but got a line."
    RawCircle _ ->
      throwError $ "Expecting a point but got a circle."


asLine :: Input a -> CommandM Line
asLine inp = do
  MapObject rawObj <- inputToMapObject inp
  case rawObj of
    RawPoint _ ->
      throwError $ "Expecting a line but got a point."
    RawLine l ->
      return l
    RawCircle _ ->
      throwError $ "Expecting a line but got a circle."


asCircle :: Input a -> CommandM Circle
asCircle inp = do
  MapObject rawObj <- inputToMapObject inp
  case rawObj of
    RawPoint _ ->
      throwError $ "Expecting a circle but got a point."
    RawLine _ ->
      throwError $ "Expecting a circle but got a line."
    RawCircle c ->
      return c

assign :: Name -> MapObject -> CommandM String
assign name mapObj@(MapObject rawObj) = do
  modify (M.insert name mapObj)
  return $ "Now the name " ++ name ++ " is assigned to\n" ++ show rawObj

isFree :: Name -> CommandM Bool
isFree = fmap not . gets . M.member

getMapObject :: Name -> CommandM MapObject
getMapObject name =
  (gets $ M.lookup name) >>= \case
    Nothing ->
      throwError $ "Object with name " ++ name ++ " does not exist"
    Just mapObj ->
      return mapObj

checkFree :: Name -> CommandM ()
checkFree name = do
  free <- isFree name
  when (not free) (throwError $ name ++ " is not free.")

checkOccupied :: Name -> CommandM ()
checkOccupied name = do
  free <- isFree name
  when free (throwError $ name ++ " is not being used.")

assignResult :: Capture -> Result MapObject -> CommandM String
assignResult capture result =
  case (capture, result) of
    (_, None) ->
      throwError "No value to capture."
    (_, Infinite) ->
      throwError "Infinitely many values to capture."
    (Single name, Unique obj) ->
      checkFree name >> assign name obj
    (Single _, Distinct _ _) ->
      throwError "Cannot assign a single name to two values."
    (Pair name1 name2, Distinct obj1 obj2) -> do
      when (name1 == name2) (throwError "Names should be distint")
      checkFree name1
      checkFree name2
      msg1 <- assign name1 obj1
      msg2 <- assign name2 obj2
      return $ msg1 ++ "\n" ++ msg2
    (Pair _ _, Unique _) ->
      throwError "Will not assign two names to a single value."

assignObject :: Name -> MapObject -> CommandM String
assignObject name obj =
  checkFree name >> assign name obj

discardName :: Name -> CommandM String
discardName name = do
  free <- isFree name
  when free (throwError $ name ++ " is not being used.")
  modify $ M.delete name
  return $ "Object named " ++ name ++ " is discarded."

displayName :: Name -> CommandM String
displayName name = do
  MapObject rawObj <- getMapObject name
  return $ show rawObj
