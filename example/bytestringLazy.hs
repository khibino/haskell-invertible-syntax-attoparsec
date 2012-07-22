{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

import BinTerm

import Data.ByteString.Lazy.Char8 (ByteString, unpack)

import Text.Syntax.Check.Attoparsec.ByteString (printParseIsoByteStringChar8)
import Text.Syntax.Printer.ByteString(runPolyPrinterChar8)

exprPPIso :: ByteString -> Either String Exp
exprPPIso =  printParseIsoByteStringChar8 expr

programPPIso :: ByteString -> Either String Program
programPPIso =  printParseIsoByteStringChar8 program

showResult :: BinSyntax a -> Either String a -> String
showResult syn =  d where
  d (Right r) = "Good isomorphism syntax:\n" ++
                unpack (fromRight (runPolyPrinterChar8 syn r)) ++ "\n"
  d (Left  e) = e
  fromRight (Right r) = r

main :: IO ()
main =  mapM_ putStrLn
        (map (showResult expr    . exprPPIso   ) allExpTests ++
         map (showResult program . programPPIso) allProgTests)
