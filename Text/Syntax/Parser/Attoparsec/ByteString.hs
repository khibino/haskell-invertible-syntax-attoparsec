{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Syntax.Parser.Attoparsec.ByteString (
  runParser, runLazyParser,
  runParserChar8, runLazyParserChar8
  ) where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly
  ((<||>), TryAlternative (try, (<|>)), Syntax(..),
   RunParser)

import Data.Attoparsec.Types (Parser, IResult (..))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.Attoparsec.ByteString as A (anyWord8, try, parse)
import qualified Data.Attoparsec.ByteString.Char8 as A (anyChar)
import qualified Data.Attoparsec.ByteString.Lazy as L

import Data.Word (Word8)


instance TryAlternative (Parser ByteString) where
  try = A.try
  p <|> q = try p <||> q

instance Syntax Word8 (Parser ByteString) where
  token = A.anyWord8

runParser :: RunParser Word8 ByteString a ([String], String)
runParser parser tks =
  case A.parse parser tks of
    Fail _ estack msg -> Left (estack, msg)
    Partial _         -> Left ([], "runAttoparsec: incomplete input")
    Done _ r          -> Right r

runLazyParser :: RunParser Word8 L.ByteString a ([String], String)
runLazyParser parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r


instance Syntax Char (Parser ByteString) where
  token = A.anyChar

runParserChar8 :: RunParser Char ByteString a ([String], String)
runParserChar8 parser tks =
  case A.parse parser tks of
    Fail _ estack msg -> Left (estack, msg)
    Partial _         -> Left ([], "runAttoparsec: incomplete input")
    Done _ r          -> Right r

runLazyParserChar8 :: RunParser Char L.ByteString a ([String], String)
runLazyParserChar8 parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r

