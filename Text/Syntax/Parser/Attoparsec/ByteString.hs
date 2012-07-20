{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Syntax.Parser.Attoparsec.ByteString (
  runPolyParser', runPolyParser,
  runPolyParserChar8', runPolyParserChar8
  ) where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly
  ((<||>), TryAlternative (try, (<|>)), Syntax(..),
   RunParser)

import Data.Attoparsec.Types (Parser, IResult (..))

import Data.ByteString (ByteString, empty)
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

runResult :: IResult ByteString a -> Either ([String], String) a
runResult r' = case r' of
  Fail _ estack msg -> Left (estack, msg)
  Partial f         -> runResult (f empty)
  Done _ r          -> Right r


runPolyParser' :: RunParser Word8 ByteString a ([String], String)
runPolyParser' parser tks = runResult $ A.parse parser tks

runPolyParser :: RunParser Word8 L.ByteString a ([String], String)
runPolyParser parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r


instance Syntax Char (Parser ByteString) where
  token = A.anyChar

runPolyParserChar8' :: RunParser Char ByteString a ([String], String)
runPolyParserChar8' parser tks = runResult $ A.parse parser tks

runPolyParserChar8 :: RunParser Char L.ByteString a ([String], String)
runPolyParserChar8 parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r

