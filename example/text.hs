{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

import BinTerm

import Data.Text (Text, unpack)

import Text.Syntax.Check.Attoparsec.Text (printParseIsoText')
import Text.Syntax.Printer.Text(runPolyPrinter')

exprPPIso :: Text -> Either String Exp
exprPPIso =  printParseIsoText' expr

programPPIso :: Text -> Either String Program
programPPIso =  printParseIsoText' program

showResult :: BinSyntax a -> Either String a -> String
showResult syn =  d where
  d (Right r) = "Good isomorphism syntax:\n" ++
                unpack (fromRight (runPolyPrinter' syn r)) ++ "\n"
  d (Left  e) = e
  fromRight (Right r) = r

main :: IO ()
main =  mapM_ putStrLn
        (map (showResult expr    . exprPPIso   ) allExpTests ++
         map (showResult program . programPPIso) allProgTests)