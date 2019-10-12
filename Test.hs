{-# LANGUAGE OverloadedStrings #-}

import Language.OpenSCAD
import qualified Data.ByteString as BS
import qualified Data.Text as T
import System.FilePath
import Test.Tasty
import Test.Tasty.Silver

main :: IO ()
main = do
    testTree <- getTests
    defaultMain testTree

getTests :: IO TestTree
getTests = do
    cases <- findByExtension [".scad"] "tests"
    return $ testGroup "golden tests"
        [ goldenVsAction base (file <.> "parsed") (dumpScad file) T.pack
        | file <- cases
        , let base = takeBaseName file
        ]

dumpScad :: FilePath -> IO String
dumpScad file = do
    result <- parse <$> BS.readFile file
    case result of
      Left err -> fail err
      Right a  -> return $ show a
