{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Language.OpenSCAD
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.String as PP
import System.FilePath
import Text.Show.Pretty
import Text.Trifecta hiding (ident)
import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.Silver

main :: IO ()
main = do
    testTree <- getTests
    defaultMain $ testGroup "tests" [testTree, roundTripTests]

getTests :: IO TestTree
getTests = do
    cases <- findByExtension [".scad"] "tests"
    return $ testGroup "golden tests"
        [ goldenVsAction base (file <.> "parsed") (dumpScad file) T.pack
        | file <- cases
        , let base = takeBaseName file
        ]

roundTripTests :: TestTree
roundTripTests = testGroup "roundtrip tests"
  [ roundtrip "ident" ident
  , roundtrip "expression" expression
  ]
 where
   parse :: Parser a -> String -> Result a 
   parse p = parseString p mempty
   render :: PP.Pretty a => a -> String
   render = PP.renderString . PP.layoutCompact . PP.pretty
   roundtrip name p = QC.testProperty name $ \e -> parse p (render e) `isSuccess` e

isSuccess :: Eq a => Result a -> a -> Bool
(Success a) `isSuccess` b = a == b
_ `isSuccess` _ = False

dumpScad :: FilePath -> IO String
dumpScad file = do
    result <- parse <$> BS.readFile file
    case result of
      Left err -> fail err
      Right a  -> return $ ppShow a
