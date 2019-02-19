{-# LANGUAGE
    LambdaCase
#-}

module State where

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Except
import           Command
import qualified Data.Map.Strict as M

type GlobalState = M.Map Name Object
type CommandM = ExceptT String (State GlobalState)

emptyState :: GlobalState
emptyState = mempty

issue :: Command -> CommandM String
issue = undefined