{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Syntax.Parser.AttoparsecZepto where

import Data.Word (Word8)

import Control.Isomorphism.Partial (IsoFunctor (..))
import Control.Isomorphism.Partial.Unsafe (Iso(Iso))
import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class
  (TryAlternative, StreamSyntax(..), Syntax(..))

import Data.ByteString (ByteString, uncons, singleton)
import Data.Attoparsec.Zepto (Parser)
import qualified Data.Attoparsec.Zepto as Z

instance TryAlternative Parser

instance StreamSyntax ByteString Parser where
  string = Z.string

instance Syntax Word8 ByteString Parser where
  token = Iso f g <$> Z.take 1  where
    f = fmap fst . uncons
    g = Just . singleton
