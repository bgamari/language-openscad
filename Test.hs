{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Language.OpenSCAD
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.String.QQ (s)
import qualified Data.Text as T
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.String as PP
import System.FilePath
import Text.Show.Pretty
import Text.Trifecta hiding (ident)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.Silver

main :: IO ()
main = do
    testTree <- getTests
    defaultMain $ testGroup "tests" [testTree, roundTripTests, prettyTests, parseTests]

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
  , roundtrip "object" object
  , roundtrip "topLevel" topLevel
  ]
 where
   parse :: HasCallStack => Parser a -> String -> Result a 
   parse p = parseString p mempty
   render :: (HasCallStack, PP.Pretty a) => a -> String
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

prettyTests :: TestTree
prettyTests = testGroup "pretty tests"
  [ testGroup "Module"
    [ testFormat 80 [s|
myModule();|]
    , testFormat 10 [s|
myModule();|]
    , testFormat 80 [s|
myModule(arg1);|]
    , testFormat 80 [s|
myModule(arg1, arg2);|]
    , testFormat 80 [s|
myModule(arg1 = 1.0, arg2);|]
    , testFormat 80 [s|
myModule() myModule2();|]
    , testFormat 20 [s|
myModule( arg1
        , arg2 );|]
    , testFormat 20 [s|
myModule()
  myModule2();|]
    , testFormat 80 [s|
myModule() { foo(); }|]
    , testFormat 80 [s|
myModule() { foo(); bar(); }|]
    , testFormat 15 [s|
myModule() {
  foo();
  bar();
  baz();
}|]
    ]
  , testGroup "ForLoop"
    [ testFormat 80 [s|
for(myVar = 1.0) myModule();|]
    , testFormat 20 [s|
for(myVar = 1.0)
  myModule();|]
    ]
    , testFormat 80 [s|
for(myVar = true) { foo(); bar(); }|]
    , testFormat 15 [s|
for(myVar = true) {
  foo();
  bar();
  baz();
}|]
  , testGroup "Objects"
    [ testFormat 80 [s|
{ myModule1(); myModule2(); }|]
    , testFormat 20 [s|
{
  myModule1();
  myModule2();
}|]
    ]
  , testGroup "If"
    [
      testFormat 80 [s|
if (true) myModule();|]
    , testFormat 20 [s|
if (true)
  myModule();|]
    , testFormat 80 [s|
if (true) { myModule(); myModule(); }|]
    , testFormat 20 [s|
if (true) {
  myModule();
  myModule();
}|]
    ]
  , testGroup "*Mod"
    [ testFormat 80 [s|
%myModule();|]
    , testFormat 80 [s|
#myModule();|]
    , testFormat 80 [s|
!myModule();|]
    , testFormat 80 [s|
*myModule();|]
    ]
  , testGroup "ModuleDef"
    [ testFormat 80 [s|
module myModule() {}|]
    , testFormat 10 [s|
module myModule() {}|]
    , testFormat 80 [s|
module myModule(arg1, arg2, arg3 = true) {}|]
    , testFormat 10 [s|
module myModule( arg1
               , arg2
               , arg3 = true ) {}|]
    , testFormat 80 [s|
module myModule() { myModule2(); myModule2(); }|]
    , testFormat 20 [s|
module myModule() {
  myModule2();
  myModule2();
}|]
    ]
  , testGroup "VarDef"
    [ testFormat 80 [s|
myVar = true;|]
    , testFormat 10 [s|
myVar
  = true;|]
    ]
  , testGroup "FuncDef"
    [ testFormat 80 [s|
function myFunc() = true;|]
    , testFormat 10 [s|
function myFunc()
  = true;|]
    , testFormat 80 [s|
function myFunc(arg1, arg2) = true;|]
    , testFormat 10 [s|
function myFunc( arg1
               , arg2 )
  = true;|]
    ]
  , testGroup "Expr"
    [ testFormat 80 [s|
myVar = myVar2;|]
    , testFormat 80 [s|
myVar = myVar2[foo];|]
    , testFormat 80 [s|
myVar = 1.0;|]
    , testFormat 80 [s|
myVar = [true, false];|]
    , testFormat 20 [s|
myVariables
  = [aaa, b];|]
    , testFormat 10 [s|
myVar
  = [ true
    , false ];|]
    ]
    , testFormat 80 [s|
myVar = [true:false];|]
    , testFormat 25 [s|
myVariables
  = [true:false];|]
    , testFormat 15 [s|
myVar
  = [ true
    : false ];|]
    , testFormat 80 [s|
myVar = "myString";|]
    , testFormat 80 [s|
myVar = "myString\"";|]
    -- TODO: string escapings
    , testFormat 80 [s|
myVar = true;|]
    , testFormat 80 [s|
myVar = false;|]
    , testFormat 80 [s|
myVar = myFunc();|]
    , testFormat 80 [s|
myVar = myFunc(arg1, arg2);|]
    , testFormat 25 [s|
myVar
  = myFunc(arg1, arg2);|]
    , testFormat 20 [s|
myVar
  = myFunc( arg1
          , arg2 );|]
    , testFormat 80 [s|
myVar = ! myVar2;|] -- FIXME
    , testFormat 80 [s|
myVar = a + b;|]
    , testFormat 15 [s|
myVariable
  = aaaaa + b;|]
    , testFormat 10 [s|
myVarable
  = aaaaa
    + b;|]
    , testFormat 80 [s|
myVar = a ? b : c;|]
    , testFormat 15 [s|
myVar
  = a ? b : c;|]
    , testFormat 10 [s|
myVar
  = aaaa
    ? b
    : c;|]
    , testFormat 80 [s|
myVar = (a + b);|]
    , testFormat 15 [s|
myVariable
  = (a + b);|]
    , testFormat 10 [s|
myVar
  = ( aaaaa
      + b
      + c );|]
  ]
 where
   testFormat :: HasCallStack => Int -> String -> TestTree
   testFormat w src = testCase src $ case parse (BS8.pack src) of 
     Left e -> assertFailure $ "Parse failure: " <> e
     Right [tl] ->
       let opts = PP.LayoutOptions $ PP.AvailablePerLine w 1
       in PP.renderString (PP.layoutPretty opts $ PP.pretty tl) @?= src
     Right tls -> assertFailure $
      "Parse failure: expected single TopLevel but got:\n" <> show (PP.vsep $ PP.pretty <$> tls)

parseTests :: TestTree
parseTests = testGroup "parse tests"
  [ testParse expression "a[b]" $ EIndex (EVar (Ident "a")) (EVar (Ident "b"))
  , testParse expression "a[b][c]" $ EIndex (EIndex (EVar (Ident "a")) (EVar (Ident "b"))) (EVar (Ident "c"))
  ]
  where
    testParse :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
    testParse p s v = testCase s $ case parseString p mempty s of
      Failure e -> assertFailure $ "Parse failure: " <> show e
      Success v' -> v' @?= v
