module Main where

import qualified Data.ByteString.Char8 as BS
import           Language.OpenSCAD
import           System.Environment
import           Text.Show.Pretty      (ppShow)

main :: IO ()
main = do
  args <- getArgs
  scad <- dumpScad (head args)
  putStrLn scad

dumpScad :: FilePath -> IO String
dumpScad file = do
  result <- parse <$> BS.readFile file
  case result of
    Left err -> fail err
    Right a  -> return $ ppShow a
