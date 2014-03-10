{-# LANGUAGE OverloadedStrings #-}                

import Language.OpenSCAD
import Data.Attoparsec.Char8
import Data.Attoparsec
import qualified Data.ByteString as BS
import System.Environment
import System.Exit
       
main = do
    file <- head `fmap` getArgs
    example <- BS.readFile file
    let result = parseFile example
    case result of
      Left error -> putStrLn error >> exitWith (ExitFailure 1)
      Right a    -> print a
