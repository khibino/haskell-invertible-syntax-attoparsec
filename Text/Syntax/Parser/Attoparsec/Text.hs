{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Syntax.Parser.Attoparsec.Text (
  runParser, runLazyParser
  ) where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly
  ((<||>), TryAlternative (try, (<|>)), Syntax(..),
   RunParser)

import Data.Attoparsec.Types (Parser, IResult (..))

import Data.Text (Text)
import qualified Data.Text.Lazy as L (Text)
import qualified Data.Attoparsec.Text as A (anyChar, try, parse)
import qualified Data.Attoparsec.Text.Lazy as L

instance TryAlternative (Parser Text) where
  try = A.try
  p <|> q = try p <||> q

instance Syntax Char (Parser Text) where
  token = A.anyChar

runParser :: RunParser Char Text a ([String], String)
runParser parser tks =
  case A.parse parser tks of
    Fail _ estack msg -> Left (estack, msg)
    Partial _         -> Left ([], "runAttoparsec: incomplete input")
    Done _ r          -> Right r

runLazyParser :: RunParser Char L.Text a ([String], String)
runLazyParser parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r
