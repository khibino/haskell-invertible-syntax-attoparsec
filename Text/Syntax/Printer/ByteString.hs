{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Syntax.Printer.ByteString (
  runAsPrinter, runAsPrinterChar8,
  runAsPrinter', runAsPrinterChar8'
  ) where

import Control.Monad (liftM2, mplus)

import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Text.Syntax.Poly
  (ProductFunctor ((<*>)),
   IsoAlternative ((<||>), empty), TryAlternative,
   AbstractSyntax (syntax), Syntax (token),
   RunAsPrinter, ErrorString, errorString)

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

runAsPrinter :: RunAsPrinter Word8 ByteString a ErrorString
runAsPrinter printer x = maybe
                           (Left . errorString $ "print error")
                           Right
                           $ runPrinter printer x

instance Syntax Char Printer where
  token  = Printer $ Just . C.singleton

runAsPrinterChar8 :: RunAsPrinter Char ByteString a ErrorString
runAsPrinterChar8 printer x = maybe
                           (Left . errorString $ "print error")
                           Right
                           $ runPrinter printer x


l2s :: ByteString -> S.ByteString
l2s =  S.concat . toChunks

runAsPrinter' :: RunAsPrinter Word8 S.ByteString a ErrorString
runAsPrinter' printer = (l2s `fmap`) . runAsPrinter printer

runAsPrinterChar8' :: RunAsPrinter Char S.ByteString a ErrorString
runAsPrinterChar8' printer = (l2s `fmap`) . runAsPrinterChar8 printer
