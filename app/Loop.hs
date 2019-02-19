{-# LANGUAGE
    LambdaCase
#-}

module Loop where

import           System.Console.Haskeline
import           Control.Monad.Morph (generalize, hoist)
import qualified Text.Megaparsec as MP
import           Command (commandParser)
import           State (emptyState, issue, _runCommandM)
import           Control.Monad.State.Strict
import           Control.Monad.Except

loop :: IO ()
loop = flip evalStateT emptyState $
       runInputT defaultSettings $
       initial >> update
  where
    initial =
      outputStrLn initialMessage
    update =
      getInputLine "> " >>= \case
        Nothing -> do
          outputStrLn "Exiting..."
          return ()
        Just str -> do
          case MP.parse commandParser "" str of
            Left bundle ->
              outputStrLn $ MP.errorBundlePretty bundle
            Right command ->
              (lift $ hoist generalize $ runExceptT $ _runCommandM $ issue command) >>= \case
                Left err ->
                  outputStrLn $ "!!! " ++ err
                Right msg ->
                  outputStrLn msg
          update

initialMessage :: String
initialMessage =
  "Welcome to Straightedge and Compass!\n" ++
  "Use CTRL + D to exit."