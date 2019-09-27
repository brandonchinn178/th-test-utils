{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Data.Void (Void)
import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.TH.TestUtils
import TH

main :: IO ()
main = defaultMain $ testGroup "th-test-utils"
  [ testFirstConstrForType
  , testExplode
  ]

testFirstConstrForType :: TestTree
testFirstConstrForType = testGroup "firstConstrForType"
  [ testCase "tryQ Maybe" $
    $(tryQ $ firstConstrForType "Maybe") @?= (Right "Nothing" :: Either String String)
  , testCase "tryQErr Maybe" $
    $(tryQErr $ firstConstrForType "Maybe") @?= (Nothing :: Maybe String)
  , testCase "tryQ NonExistent" $
    $(tryQ $ firstConstrForType "NonExistent") @?= (Left "Type does not exist: NonExistent" :: Either String String)
  , testCase "tryQErr Show" $
    $(tryQErr $ firstConstrForType "Show") @?= Just "Not a data type: Show"
  , testCase "tryQErr' Void" $
    $(tryQErr' $ firstConstrForType "Void") @?= "Data type has no constructors: Void"
  ]

testExplode :: TestTree
testExplode = testGroup "explode"
  [ testCase "abc" $
    $(tryQ $ explode "abc") @?= (Right ["a", "b", "c"] :: Either String [String])
  , testCase "\"\"" $
    $(tryQ $ explode "") @?= (Left "Cannot explode empty string" :: Either String [String])
  ]
