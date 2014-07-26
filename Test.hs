{-# LANGUAGE OverloadedStrings #-}                

import Language.OpenSCAD
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import System.Environment
import System.Exit
import System.IO
       
main = do
    file <- head `fmap` getArgs
    example <- BS.readFile file
    let result = parseFile example
    case result of
      Left error
        | length error > 50 -> putStrLn error >> exitWith (ExitFailure 1)
        | otherwise         -> putStrLn $ "warning: "++error
      Right a    -> print a
