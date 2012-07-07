{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Syntax.Printer.ByteString where

import Control.Monad (liftM2, mplus)

import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Text.Syntax.Poly.Class
  (ProductFunctor ((<*>)),
   IsoAlternative ((<||>), empty), TryAlternative,
   AbstractSyntax (syntax), StreamSyntax (string), Syntax (token))

import Data.ByteString (ByteString, append)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)

newtype Printer alpha =
  Printer { runPrinter :: alpha -> Maybe ByteString }

instance IsoFunctor Printer where
  iso <$> Printer p
    = Printer (\b -> unapply iso b >>= p)

instance ProductFunctor Printer where
  Printer p <*> Printer q
    = Printer (\(x, y) -> liftM2 append (p x) (q y))

instance IsoAlternative Printer where
  Printer p <||> Printer q
    = Printer (\s -> mplus (p s) (q s))
  empty = Printer (\_ -> Nothing)

instance TryAlternative Printer

instance AbstractSyntax Printer where
  syntax x = Printer (\y ->  if x == y
                             then Just B.empty
                             else Nothing)

instance StreamSyntax ByteString Printer where
  string s = Printer (\() -> Just s)

instance Syntax Word8 ByteString Printer where
  token  = Printer $ Just . B.singleton

instance Syntax Char ByteString Printer where
  token  = Printer $ Just . C.singleton
