{-# LANGUAGE
    LambdaCase
  , GADTs
#-}

module State where

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class (lift)
import           Command
import           Geometry
import qualified Data.Map.Strict as M

data Object
  = ObjPoint Point
  | ObjLine Line
  | ObjCircle Circle
  deriving Show

type GlobalState = M.Map String Object
type CommandM = ExceptT String (State GlobalState)

emptyState :: GlobalState
emptyState = M.empty

issue :: Command -> CommandM String
issue = \case
  Construct construction ->
    show . snd <$> evalConstruction construction
  Name construction capture -> do
    result <- evalConstruction construction
    case (capture, result) of
      (_, _) ->
        undefined
  Discard name ->
    undefined
  Show name ->
    undefined

evalConstruction :: Construction a -> CommandM (a, String)
evalConstruction = \case
  LineLineIntersection inp1 inp2 -> do
    l1 <- evalAsLine inp1
    l2 <- evalAsLine inp2
    let cons = intersectionOfLines l1 l2
    return (intersectionOfLines l1 l2, "Constructed:\n" ++ show cons)

evalAsLine :: Input Line -> CommandM Line
evalAsLine = undefined