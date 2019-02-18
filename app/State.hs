module State where

import           Control.Monad.Trans.State.Strict
import           Command

data GlobalState = GlobalState

emptyState :: GlobalState
emptyState = GlobalState

issue :: Command -> State GlobalState String
issue _ = return "Success"