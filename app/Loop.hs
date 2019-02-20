{-# LANGUAGE
    LambdaCase
#-}

module Loop where

import           Control.Monad.Except (runExceptT)
import           Control.Monad.State.Strict (evalStateT, lift)
import           Control.Monad.Morph (generalize, hoist)
import           System.Console.Haskeline (runInputT, getInputLine, outputStrLn, defaultSettings)
import           Text.Megaparsec (parse, errorBundlePretty)
-------------------------------------------------------------------------------
import           Command (commandParser)
import           State (emptyState, issue, _runCommandM)

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
          case parse commandParser "" str of
            Left bundle ->
              outputStrLn $ errorBundlePretty bundle
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