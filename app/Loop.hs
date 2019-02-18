{-# LANGUAGE
    LambdaCase
#-}

module Loop where

import           System.Console.Haskeline
import           Control.Monad (forever)

loop :: IO ()
loop = runInputT defaultSettings $ initial >> forever update
  where
    initial =
      outputStrLn "Welcome to Straightedge and Compass!"
    update =
      getInputLine "> " >>= \case
        Nothing -> return ()
        Just str -> outputStrLn $ "echo: " ++ str

