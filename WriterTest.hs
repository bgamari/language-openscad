{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8                 as BS
import           Data.Text
import qualified Data.Text.Prettyprint.Doc             as P
import           Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import           System.IO

import           Language.OpenSCAD
import           Language.OpenSCAD.Writer

main :: IO ()
main = do
  scad <- BS.readFile "test.scad"
  let layoutOptions =
        P.LayoutOptions {P.layoutPageWidth = P.AvailablePerLine 80 1}
  case parse scad of
    Left err -> putStrLn err
    Right tl ->
      renderIO System.IO.stdout $
      P.layoutPretty layoutOptions $ P.unAnnotate $ pretty tl
  return ()
