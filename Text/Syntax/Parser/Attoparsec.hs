{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Syntax.Parser.Attoparsec () where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative (try, (<|>)), StreamSyntax(..), Syntax(..))

import Data.Text (Text)
import qualified Data.Text as T (empty)
import qualified Data.Attoparsec.Text as AT

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as AB (anyWord8)
import qualified Data.Attoparsec.ByteString.Char8 as AB

import Data.Attoparsec.Types (Parser)

import Data.Word (Word8)

instance TryAlternative (Parser Text) where
  try = AT.try

instance StreamSyntax Text (Parser Text) where
  string s = AT.string s >> return ()

instance Syntax Char Text (Parser Text) where
  token = AT.anyChar


instance TryAlternative (Parser ByteString) where
  try = AB.try

instance StreamSyntax ByteString (Parser ByteString) where
  string s = AB.string s >> return ()

instance Syntax Word8 ByteString (Parser ByteString) where
  token = AB.anyWord8

instance Syntax Char ByteString (Parser ByteString) where
  token = AB.anyChar
