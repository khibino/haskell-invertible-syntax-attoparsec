{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Text.Syntax.Check.Attoparsec.Text (
  printParseIsoText0,
  printParseIsoText,

  printParseIsoText0',
  printParseIsoText'
 ) where

import Data.Text.Lazy (Text)
import qualified Data.Text as S (Text)

import Text.Syntax.Poly (SyntaxT)
import Text.Syntax.Check.Prim
  (printParseIso', printParseIso)

import Text.Syntax.Parser.Attoparsec.Text (runPolyParser, runPolyParser')
import Text.Syntax.Printer.Text (runPolyPrinter, runPolyPrinter')


printParseIsoText0 :: Eq a => SyntaxT Char a -> a -> Either String a
printParseIsoText0 =  printParseIso' runPolyPrinter runPolyParser

printParseIsoText :: Eq a => SyntaxT Char a -> Text -> Either String a
printParseIsoText =  printParseIso runPolyPrinter runPolyParser

printParseIsoText0' :: Eq a => SyntaxT Char a -> a -> Either String a
printParseIsoText0' =  printParseIso' runPolyPrinter' runPolyParser'

printParseIsoText' :: Eq a => SyntaxT Char a -> S.Text -> Either String a
printParseIsoText' =  printParseIso runPolyPrinter' runPolyParser'
