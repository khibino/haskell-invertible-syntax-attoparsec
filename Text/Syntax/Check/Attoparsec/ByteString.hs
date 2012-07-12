{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Text.Syntax.Check.Attoparsec.ByteString (
  printParseIsoByteString0,
  printParseIsoByteString,

  printParseIsoByteString0',
  printParseIsoByteString',

  printParseIsoByteString0Char8,
  printParseIsoByteStringChar8,

  printParseIsoByteString0Char8',
  printParseIsoByteStringChar8'
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as S (ByteString)
import Data.Word (Word8)

import Text.Syntax.Poly (SyntaxT)
import Text.Syntax.Check.Prim
  (printParseIso', printParseIso)

import Text.Syntax.Parser.Attoparsec.ByteString
  (runPolyParser, runPolyParser', runPolyParserChar8, runPolyParserChar8')
import Text.Syntax.Printer.ByteString
  (runPolyPrinter, runPolyPrinter', runPolyPrinterChar8, runPolyPrinterChar8')


printParseIsoByteString0 :: Eq a => SyntaxT Word8 a -> a -> Either String a
printParseIsoByteString0 =  printParseIso' runPolyPrinter runPolyParser

printParseIsoByteString :: Eq a => SyntaxT Word8 a -> ByteString -> Either String a
printParseIsoByteString =  printParseIso runPolyPrinter runPolyParser

printParseIsoByteString0' :: Eq a => SyntaxT Word8 a -> a -> Either String a
printParseIsoByteString0' =  printParseIso' runPolyPrinter' runPolyParser'

printParseIsoByteString' :: Eq a => SyntaxT Word8 a -> S.ByteString -> Either String a
printParseIsoByteString' =  printParseIso runPolyPrinter' runPolyParser'


printParseIsoByteString0Char8 :: Eq a => SyntaxT Char a -> a -> Either String a
printParseIsoByteString0Char8 =  printParseIso' runPolyPrinterChar8 runPolyParserChar8

printParseIsoByteStringChar8 :: Eq a => SyntaxT Char a -> ByteString -> Either String a
printParseIsoByteStringChar8 =  printParseIso runPolyPrinterChar8 runPolyParserChar8

printParseIsoByteString0Char8' :: Eq a => SyntaxT Char a -> a -> Either String a
printParseIsoByteString0Char8' =  printParseIso' runPolyPrinterChar8' runPolyParserChar8'

printParseIsoByteStringChar8' :: Eq a => SyntaxT Char a -> S.ByteString -> Either String a
printParseIsoByteStringChar8' =  printParseIso runPolyPrinterChar8' runPolyParserChar8'
