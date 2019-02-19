{-# LANGUAGE
    LambdaCase
#-}

module State where

import           Control.Monad.Trans.State.Strict
import           Command
import qualified Data.Map.Strict as M

type GlobalState = M.Map Name Object
type CommandM = State GlobalState

emptyState :: GlobalState
emptyState = mempty


checkName
  :: Name
  -> CommandM String
  -> (Object -> CommandM String)
  -> CommandM String
checkName name action1 action2 =
  (gets $ M.lookup name) >>= \case
    Nothing ->
      action1
    Just obj ->
      action2 obj

issue :: Command -> State GlobalState String
issue = \case
  Construct obj ->
    return $ show obj

  Assign obj name ->
    checkName name
      (modify (M.insert name obj) >> return "Object assigned.")
      (return . ("Object already exists:\n" ++) . show)

  Discard name ->
    checkName name
      (return $ "No object with name " ++ name ++ " exists.")
      (const $  modify (M.delete name) >> return "Object discarded.")

  Show name ->
    checkName name
      (return $ "No object with name " ++ name ++ " exists.")
      (return . show)