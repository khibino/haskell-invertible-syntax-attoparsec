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
  (printParseIso0, printParseIso)

import Text.Syntax.Parser.Attoparsec.Text (runAsAttoparsec, runAsAttoparsec')
import Text.Syntax.Printer.Text (runAsPrinter, runAsPrinter')


printParseIsoText0 :: Eq a => SyntaxT Char a -> a -> Either String a
printParseIsoText0 =  printParseIso0 runAsPrinter runAsAttoparsec

printParseIsoText :: Eq a => SyntaxT Char a -> Text -> Either String a
printParseIsoText =  printParseIso runAsPrinter runAsAttoparsec

printParseIsoText0' :: Eq a => SyntaxT Char a -> a -> Either String a
printParseIsoText0' =  printParseIso0 runAsPrinter' runAsAttoparsec'

printParseIsoText' :: Eq a => SyntaxT Char a -> S.Text -> Either String a
printParseIsoText' =  printParseIso runAsPrinter' runAsAttoparsec'
