{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Syntax.Parser.Attoparsec.Zepto where

import Data.Word (Word8)

import Control.Isomorphism.Partial (IsoFunctor (..))
import Control.Isomorphism.Partial.Unsafe (Iso(Iso))
import Text.Syntax.Parser.Instances ()
import Text.Syntax.Poly.Class (TryAlternative, Syntax(..))

import qualified Data.ByteString as B (uncons, singleton)
import qualified Data.ByteString.Char8 as C8 (uncons, singleton)
import Data.Attoparsec.Zepto (Parser)
import qualified Data.Attoparsec.Zepto as Z

instance TryAlternative Parser

instance Syntax Word8 Parser where
  token = Iso f g <$> Z.take 1  where
    f = fmap fst . B.uncons
    g = Just . B.singleton

instance Syntax Char Parser where
  token = Iso f g <$> Z.take 1  where
    f = fmap fst . C8.uncons
    g = Just . C8.singleton
