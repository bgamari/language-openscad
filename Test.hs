{-# LANGUAGE OverloadedStrings #-}                

import Language.OpenSCAD
import Data.Attoparsec.Char8
import Data.Attoparsec
import qualified Data.ByteString as BS
       
main = do
    example <- BS.readFile "hi.scad"
    print $ parseOnly (many1 parseTopLevel) example
    --let example = "(hi=5)"
    --print $ parseOnly (arguments) example
