module TestProperties where

import           Data.ByteString.Char8    (pack)
import           Debug.Trace
import           Language.OpenSCAD
import           Language.OpenSCAD.Writer
import           Test.QuickCheck
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.QuickCheck    (testProperty)

propertyTests :: TestTree
propertyTests =
  testGroup
    "property tests"
    [testProperty "Pretty-print and parse TopLevel" prop_prettyAndParseTopLevel]

prop_prettyAndParseTopLevel :: TopLevel -> Bool
prop_prettyAndParseTopLevel use =
  Right [use] `tracedPropEq` (parse $ pack $ show $ pretty [use])

instance Arbitrary TopLevel where
  arbitrary = oneof [nonCaretASCIIString >>= return . UseDirective]

nonCaretASCIIString :: Gen String
nonCaretASCIIString =
  listOf1 $ elements $ ['!' .. '\59'] ++ ['\61'] ++ ['\63' .. '\126']

tracedPropEq :: (Show a, Eq a) => a -> a -> Bool
tracedPropEq = tracedProp (==) "EQUAL"

tracedProp :: Show a => (a -> a -> Bool) -> String -> a -> a -> Bool
tracedProp fn s t1 t2 =
  fn t1 t2 ||
  trace
    ("=====================================" ++
     "=====================================\n" ++
     show t1 ++
     "\n" ++
     " - DOES NOT " ++
     s ++
     " -\n" ++
     show t2 ++
     "\n" ++
     "=====================================" ++
     "=====================================")
    False
