module Command where

import           Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP


data Command = Command

type Parser = MP.Parsec Void String

commandParser :: Parser Command
commandParser = MP.string "Command" >> MP.eof >> return Command