{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.Syntax.Parser.Attoparsec.Text (
  runAsAttoparsec', runAsAttoparsec
  ) where

import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly
  ((<||>), TryAlternative (try, (<|>)), Syntax(..),
   RunAsParser)

import Data.Attoparsec.Types (Parser, IResult (..))

import Data.Text (Text, empty)
import qualified Data.Text.Lazy as L (Text)
import qualified Data.Attoparsec.Text as A (anyChar, try, parse)
import qualified Data.Attoparsec.Text.Lazy as L

instance TryAlternative (Parser Text) where
  try = A.try
  p <|> q = try p <||> q

instance Syntax Char (Parser Text) where
  token = A.anyChar

runAsAttoparsec' :: RunAsParser Char Text a ([String], String)
runAsAttoparsec' parser tks = runResult $ A.parse parser tks where
  runResult r' = case r' of
    Fail _ estack msg -> Left (estack, msg)
    Partial f         -> runResult (f empty)
    Done _ r          -> Right r

runAsAttoparsec :: RunAsParser Char L.Text a ([String], String)
runAsAttoparsec parser tks =
  case L.parse parser tks of
    L.Fail _ estack msg -> Left (estack, msg)
    L.Done _ r          -> Right r
