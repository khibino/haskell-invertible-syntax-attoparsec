{-# LANGUAGE MultiParamTypeClasses #-}

module Text.Syntax.Printer.Text (
  runPolyPrinter, runPolyPrinter'
  ) where

import Control.Monad (liftM2, mplus)

import Control.Isomorphism.Partial (IsoFunctor ((<$>)), unapply)
import Text.Syntax.Poly
  (ProductFunctor ((<*>)),
   IsoAlternative ((<||>), empty), TryAlternative,
   AbstractSyntax (syntax), Syntax (token),
   RunPrinter, ErrorString, errorString)


import qualified Data.Text as S (Text, concat)
import Data.Text.Lazy (Text, append, toChunks, singleton)
import qualified Data.Text.Lazy as L

newtype Printer alpha =
  Printer { runPrinter :: alpha -> Maybe Text }

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
                             then Just L.empty
                             else Nothing)

instance Syntax Char Printer where
  token  = Printer $ Just . singleton

runPolyPrinter :: RunPrinter Char Text a ErrorString
runPolyPrinter printer x = maybe
                           (Left . errorString $ "print error")
                           Right
                           $ runPrinter printer x


l2s :: Text -> S.Text
l2s =  S.concat . toChunks

runPolyPrinter' :: RunPrinter Char S.Text a ErrorString
runPolyPrinter' printer = (l2s `fmap`) . runPolyPrinter printer
