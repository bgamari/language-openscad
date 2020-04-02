{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString.Char8 as BS

import Language.OpenSCAD
import Language.OpenSCAD.Writer

main :: IO ()
main = do
    scad <- BS.readFile "test.scad"
    putStrLn $ case parse scad of
        Left err -> err
        Right tl -> show $ pretty tl
    return ()
