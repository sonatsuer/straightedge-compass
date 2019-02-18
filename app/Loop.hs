{-# LANGUAGE
    LambdaCase
#-}

module Loop where

import           System.Console.Haskeline
import           Control.Monad (forever)
import           Control.Monad.Morph (generalize, hoist)
import qualified Text.Megaparsec as MP
import           Command (commandParser)
import           State (emptyState, issue, GlobalState(..))
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Class (lift)

loop :: IO ()
loop = flip evalStateT emptyState $
       runInputT defaultSettings $
       initial >> forever update
  where
    initial =
      outputStrLn "Welcome to Straightedge and Compass!"
    update =
      getInputLine "> " >>= \case
        Nothing ->
          return ()
        Just str ->
          case MP.parse commandParser "" str of
            Left bundle ->
              outputStrLn $ MP.errorBundlePretty bundle
            Right command -> do
              msg <- lift $ hoist generalize $ issue command
              outputStrLn msg