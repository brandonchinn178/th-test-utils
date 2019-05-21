{-# LANGUAGE TemplateHaskell #-}

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
  [ testCase "Maybe" $
    $(tryQ $ firstConstrForType "Maybe") @== Right "Nothing"
  , testCase "NonExistent" $
    $(tryQ $ firstConstrForType "NonExistent") @== Left "Type does not exist: NonExistent"
  , testCase "Show" $
    $(tryQErr $ firstConstrForType "Show") @?= Just "Not a data type: Show"
  , testCase "Void" $
    $(tryQErr' $ firstConstrForType "Void") @?= "Data type has no constructors: Void"
  ]

testExplode :: TestTree
testExplode = testGroup "explode"
  [ testCase "abc" $
    $(tryQ $ explode "abc") @?= (Right ["a", "b", "c"] :: Either String [String])
  , testCase "\"\"" $
    $(tryQ $ explode "") @?= (Left "Cannot explode empty string" :: Either String [String])
  ]

-- | Helper to specify the type of the splice.
(@==) :: Either String String -> Either String String -> IO ()
(@==) = (@?=)
