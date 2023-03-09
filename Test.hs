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
    defaultMain $ testGroup "tests" [testTree, roundtripTests, exactprintTests, parseTests, prettyTests]

getTests :: IO TestTree
getTests = do
    cases <- findByExtension [".scad"] "tests"
    return $ testGroup "golden tests"
        [ goldenVsAction base (file <.> "parsed") (dumpScad file) T.pack
        | file <- cases
        , let base = takeBaseName file
        ]

roundtripTests :: TestTree
roundtripTests = testGroup "roundtrip tests"
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
  [ testPretty (EString s) ("\"" <> s' <> "\"") 
  | (s', s) <- [("\\\"","\""), ("\\\\","\\"), ("\\t","\t"), ("\\n","\n"), ("\\r", "\r")]
  ]
  where
    testPretty :: (HasCallStack, PP.Pretty a) => a -> String -> TestTree
    testPretty e s = testCase s $
      PP.renderString (PP.layoutPretty PP.defaultLayoutOptions $ PP.pretty e) @?= s

exactprintTests :: TestTree
exactprintTests = testGroup "exactprint tests"
  [ testGroup "Module"
    [ testExactprint 80 [s|
myModule();|]
    , testExactprint 10 [s|
myModule();|]
    , testExactprint 80 [s|
myModule(arg1);|]
    , testExactprint 80 [s|
myModule(arg1, arg2);|]
    , testExactprint 80 [s|
myModule(arg1 = 1.0, arg2);|]
    , testExactprint 80 [s|
myModule() myModule2();|]
    , testExactprint 20 [s|
myModule( arg1
        , arg2 );|]
    , testExactprint 20 [s|
myModule()
  myModule2();|]
    , testExactprint 80 [s|
myModule() { foo(); }|]
    , testExactprint 80 [s|
myModule() { foo(); bar(); }|]
    , testExactprint 15 [s|
myModule() {
  foo();
  bar();
  baz();
}|]
    ]
  , testGroup "ForLoop"
    [ testExactprint 80 [s|
for(myVar = 1.0) myModule();|]
    , testExactprint 20 [s|
for(myVar = 1.0)
  myModule();|]
    ]
    , testExactprint 80 [s|
for(myVar = true) { foo(); bar(); }|]
    , testExactprint 15 [s|
for(myVar = true) {
  foo();
  bar();
  baz();
}|]
  , testGroup "Objects"
    [ testExactprint 80 [s|
{ myModule1(); myModule2(); }|]
    , testExactprint 20 [s|
{
  myModule1();
  myModule2();
}|]
    ]
  , testGroup "If"
    [ testExactprint 80 [s|
if (true) myModule();|]
    , testExactprint 20 [s|
if (true)
  myModule();|]
    , testExactprint 80 [s|
if (true) { myModule(); myModule(); }|]
    , testExactprint 20 [s|
if (true) {
  myModule();
  myModule();
}|]
    , testExactprint 80 [s|
if (true) if (true) foo();|]
    , testExactprint 80 [s|
if (true) { if (true) foo(); else bar(); }|]
    , testExactprint 80 [s|
if (true) { if (true) foo(); } else bar();|]
    ]
  , testGroup "*Mod"
    [ testExactprint 80 [s|
%myModule();|]
    , testExactprint 80 [s|
#myModule();|]
    , testExactprint 80 [s|
!myModule();|]
    , testExactprint 80 [s|
*myModule();|]
    ]
  , testGroup "ModuleDef"
    [ testExactprint 80 [s|
module myModule() {}|]
    , testExactprint 10 [s|
module myModule() {}|]
    , testExactprint 80 [s|
module myModule(arg1, arg2, arg3 = true) {}|]
    , testExactprint 10 [s|
module myModule( arg1
               , arg2
               , arg3 = true ) {}|]
    , testExactprint 80 [s|
module myModule() { myModule2(); myModule2(); }|]
    , testExactprint 20 [s|
module myModule() {
  myModule2();
  myModule2();
}|]
    ]
  , testGroup "VarDef"
    [ testExactprint 80 [s|
myVar = true;|]
    , testExactprint 10 [s|
myVar
  = true;|]
    ]
  , testGroup "FuncDef"
    [ testExactprint 80 [s|
function myFunc() = true;|]
    , testExactprint 10 [s|
function myFunc()
  = true;|]
    , testExactprint 80 [s|
function myFunc(arg1, arg2) = true;|]
    , testExactprint 10 [s|
function myFunc( arg1
               , arg2 )
  = true;|]
    ]
  , testGroup "Expr"
    [ testExactprint 80 [s|
myVar = myVar2;|]
    , testExactprint 80 [s|
myVar = myVar2[foo];|]
    , testExactprint 80 [s|
myVar = 1.0;|]
    , testExactprint 80 [s|
myVar = [true, false];|]
    , testExactprint 20 [s|
myVariables
  = [aaa, b];|]
    , testExactprint 10 [s|
myVar
  = [ true
    , false ];|]
    ]
    , testExactprint 80 [s|
myVar = [true:false];|]
    , testExactprint 25 [s|
myVariables
  = [true:false];|]
    , testExactprint 15 [s|
myVar
  = [ true
    : false ];|]
    , testExactprint 80 [s|
myVar = "myString";|]
    , testGroup "string escapings"
      [ testExactprint 80 (mconcat ["myVar = \"" <> s <> "\";"])
      | s <- ["\\\"", "\\\\", "\\t", "\\n", "\\r"]
      ]
    , testExactprint 80 [s|
myVar = true;|]
    , testExactprint 80 [s|
myVar = false;|]
    , testExactprint 80 [s|
myVar = myFunc();|]
    , testExactprint 80 [s|
myVar = myFunc(arg1, arg2);|]
    , testExactprint 25 [s|
myVar
  = myFunc(arg1, arg2);|]
    , testExactprint 20 [s|
myVar
  = myFunc( arg1
          , arg2 );|]
    , testExactprint 80 [s|
myVar = !myVar2;|]
    , testExactprint 80 [s|
myVar = -myVar2;|]
    , testExactprint 80 [s|
myVar = -(myVar2);|]
    , testExactprint 80 [s|
myVar = a + b;|]
    , testExactprint 15 [s|
myVariable
  = aaaaa + b;|]
    , testExactprint 10 [s|
myVarable
  = aaaaa
    + b;|]
    , testExactprint 80 [s|
myVar = a ? b : c;|]
    , testExactprint 15 [s|
myVar
  = a ? b : c;|]
    , testExactprint 10 [s|
myVar
  = aaaa
    ? b
    : c;|]
    , testExactprint 80 [s|
myVar = (a + b);|]
    , testExactprint 15 [s|
myVariable
  = (a + b);|]
    , testExactprint 10 [s|
myVar
  = ( aaaaa
      + b
      + c );|]
  ]
 where
   testExactprint :: HasCallStack => Int -> String -> TestTree
   testExactprint w src = testCase src $ case parse (BS8.pack src) of 
     Left e -> assertFailure $ "Parse failure: " <> e
     Right [tl] ->
       let opts = PP.LayoutOptions $ PP.AvailablePerLine w 1
       in PP.renderString (PP.layoutPretty opts $ PP.pretty tl) @?= src
     Right tls -> assertFailure $
      "Parse failure: expected single TopLevel but got:\n" <> show (PP.vsep $ PP.pretty <$> tls)

parseTests :: TestTree
parseTests = testGroup "parse tests" $ 
  [ testParse expression "a[b]" $ EIndex (EVar (Ident "a")) (EVar (Ident "b"))
  , testParse expression "a[b][c]" $ EIndex (EIndex (EVar (Ident "a")) (EVar (Ident "b"))) (EVar (Ident "c"))
  , testParse expression "-1" $ ENum (negate 1)
  , testParse expression "-(1)" $ ENegate (EParen (ENum 1))
  , testParse expression "+1" $ ENum 1
  , testParse expression "+(1)" $ EParen (ENum 1)
  , testParse expression "-1 + 2" $ EPlus (ENum (negate 1)) (ENum 2)
  -- Not supported atm
  -- , testParse expression "--1" $ ENegate (ENum (negate 1))
  -- , testParse expression "+-1" $ ENum (negate 1)
  -- , testParse expression "!-1" $ ENot (ENum (negate 1))
  ] <>
  [ testParse expression ("\"" <> s <> "\"") (EString s')
  | (s, s') <- [("\\\"","\""), ("\\\\","\\"), ("\\t","\t"), ("\\n","\n"), ("\\r", "\r")]
  ]
  where
    testParse :: (Eq a, Show a) => Parser a -> String -> a -> TestTree
    testParse p s v = testCase s $ case parseString p mempty s of
      Failure e -> assertFailure $ "Parse failure: " <> show e
      Success v' -> v' @?= v
