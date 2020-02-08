{-# LANGUAGE OverloadedStrings #-}

import Language.OpenSCAD
import Language.OpenSCAD.Writer
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import System.FilePath
import Test.Tasty
import Test.Tasty.Silver
import Text.Show.Pretty

main :: IO ()
main = do
    testTree <- getTests
    defaultMain testTree

getTests :: IO TestTree
getTests = do
    cases <- findByExtension [".scad"] "tests"
    return $ testGroup "golden tests" $
        [ goldenVsAction base (file <.> "parsed") (dumpScad file) T.pack
        | file <- cases
        , let base = takeBaseName file
        ] ++ [ goldenVsAction base file (writeScad file) T.pack
        | file <- cases
        , let base = takeBaseName file
        ]

dumpScad :: FilePath -> IO String
dumpScad file = do
    result <- parse <$> BS.readFile file
    case result of
      Left err -> fail err
      Right a  -> return $ ppShow a

writeScad :: FilePath -> IO String
writeScad file = do
    result <- parse <$> BS.readFile file
    case fmap write result of
      Left err -> fail err
      Right a  -> return $ BS.unpack $ head a
