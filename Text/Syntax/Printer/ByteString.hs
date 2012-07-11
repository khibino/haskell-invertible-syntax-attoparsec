{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Syntax.Printer.ByteString (
  runPolyLazyPrinter, runPolyLazyPrinterChar8,
  runPolyPrinter, runPolyPrinterChar8
  ) where

import Control.Monad (liftM2, mplus)

import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Text.Syntax.Poly
  (ProductFunctor ((<*>)),
   IsoAlternative ((<||>), empty), TryAlternative,
   AbstractSyntax (syntax), Syntax (token),
   RunPrinter, ErrorString, errorString)

import qualified Data.ByteString as S (ByteString, concat)
import Data.ByteString.Lazy (ByteString, append, toChunks)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
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

instance Syntax Word8 Printer where
  token  = Printer $ Just . B.singleton

runPolyLazyPrinter :: RunPrinter Word8 ByteString a ErrorString
runPolyLazyPrinter printer x = maybe
                           (Left . errorString $ "print error")
                           Right
                           $ runPrinter printer x

instance Syntax Char Printer where
  token  = Printer $ Just . C.singleton

runPolyLazyPrinterChar8 :: RunPrinter Char ByteString a ErrorString
runPolyLazyPrinterChar8 printer x = maybe
                           (Left . errorString $ "print error")
                           Right
                           $ runPrinter printer x


l2s :: ByteString -> S.ByteString
l2s =  S.concat . toChunks

runPolyPrinter :: RunPrinter Word8 S.ByteString a ErrorString
runPolyPrinter printer = (l2s `fmap`) . runPolyLazyPrinter printer

runPolyPrinterChar8 :: RunPrinter Char S.ByteString a ErrorString
runPolyPrinterChar8 printer = (l2s `fmap`) . runPolyLazyPrinterChar8 printer
