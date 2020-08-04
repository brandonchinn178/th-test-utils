{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Language.Haskell.TH (AnnLookup(..), Q, Type(..))
import Language.Haskell.TH.Syntax (Extension(..), Quasi(..))
#if MIN_VERSION_template_haskell(2,12,0)
import Language.Haskell.TH.Syntax (ForeignSrcLang(..))
#endif
import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.TH.TestUtils
import TestLib
import TH

main :: IO ()
main = defaultMain $ testGroup "th-test-utils"
  [ testAllowQ
  ]

testAllowQ :: TestTree
testAllowQ = testGroup "AllowQ"
  [ testRunners
  , testMethods
  ]
  where
    testRunners = testGroup "tryTestQ, runTestQ, runTestQErr"
      [ $(testCaseTH "tryTestQ - success" $ do
            let x = 1
            result <- tryTestQ unmockedState (return x :: Q Int)
            return $ result @?= Right x
          )
      , $(testCaseTH "tryTestQ - error" $ do
            let msg = "Error message"
            result <- tryTestQ unmockedState (fail msg :: Q Int)
            return $ result @?= Left msg
          )
      , $(testCaseTH "fmap Right . runTestQ === tryTestQ" $ do
            actual <- Right <$> runTestQ unmockedState basicSuccess
            expected <- tryTestQ unmockedState basicSuccess
            return $ actual @?= expected
          )
      , $(testCaseTH "runTestQ errors on failure" $ do
            let msg = "Error message"
            result <- tryQ $ runTestQ unmockedState (fail msg :: Q ())
            return $ result @?= Left msg
          )
      , $(testCaseTH "fmap Left . runTestQErr === tryTestQ" $ do
            actual <- Left <$> runTestQErr unmockedState basicFailure
            expected <- tryTestQ unmockedState basicFailure
            return $ actual @?= expected
          )
      , $(testCaseTH "runTestQErr errors on success" $ do
            result <- tryQ $ runTestQErr unmockedState (return 1 :: Q Int)
            return $ result @?= Left "Unexpected success: 1"
          )
      ]
    testMethods = testGroup "Quasi methods"
      [ $(testCaseTH "qNewName" $
            isSuccess $ runTestQ unmockedState $ qNewName "foo"
          )
      , $(testCaseTH "qReport" $
            -- not testing 'qReport False' because it fails with '-Werror'
            -- isSuccess $ runTestQ unmockedState $ qReport False "A warning message"
            isSuccess $ runTestQ unmockedState $ qReport True "An error message"
          )
      , $(testCaseTH "qRecover" $ do
            let x = "Success"
            result <- runTestQ unmockedState $ qRecover (return x) basicFailure
            return $ result @?= x
          )
      , $(testCaseTH "qLookupName" $
            checkUnmockedMatches $ sequence
              [ qLookupName True "Show"
              , qLookupName True "Right"
              , qLookupName False "Right"
              , qLookupName False "Show"
              ]
          )
      , $(testCaseTH "qReify" $
            checkUnmockedMatches $ qReify ''Maybe
          )
      , $(testCaseTH "qReifyFixity" $
            checkUnmockedMatches $ qReifyFixity '($)
          )
#if MIN_VERSION_template_haskell(2,16,0)
      , $(testCaseTH "qReifyType" $
            checkUnmockedMatches $ qReifyType ''Maybe
          )
#endif
      , $(testCaseTH "qReifyInstances" $
            checkUnmockedMatches $ qReifyInstances ''Show [ConT ''Int]
          )
      , $(testCaseTH "qReifyRoles" $
            checkUnmockedMatches $ qReifyRoles ''Maybe
          )
      , $(testCaseTH "qReifyAnnotations" $
            checkUnmockedMatches $ qReifyAnnotations @_ @[String] (AnnLookupName 'basicSuccess)
          )
      , $(testCaseTH "qReifyModule" $
            checkUnmockedMatches $ qReifyModule $(thisModule)
          )
      , $(testCaseTH "qReifyConStrictness" $
            checkUnmockedMatches $ qReifyConStrictness 'Just
          )
      , $(testCaseTH "qLocation" $
            checkUnmockedMatches qLocation
          )
      , $(testCaseTH "qRunIO, qAddDependentFile" $
            checkUnmockedMatches $ do
              readmeContents <- qRunIO $ readFile "README.md"
              qAddDependentFile "README.md"
              return readmeContents
          )
      , $(testCaseTH "qAddTopDecls" $ do
            decs <- [d| {-# ANN module "AllowQ - qAddTopDecls" #-} |]
            isSuccess $ runTestQ unmockedState $ qAddTopDecls decs
          )
#if MIN_VERSION_template_haskell(2,14,0)
      , $(testCaseTH "qAddTempFile, qAddForeignFilePath" $ isSuccess $ do
            fp <- runTestQ unmockedState $ qAddTempFile "c"
            qRunIO $ writeFile fp "#include <stdio.h>"
            runTestQ unmockedState $ qAddForeignFilePath LangC fp
          )
#elif MIN_VERSION_template_haskell(2,12,0)
      , $(testCaseTH "qAddForeignFile" $
            isSuccess $ runTestQ unmockedState $ qAddForeignFile LangC "#include <stdio.h>"
          )
#endif
      , $(testCaseTH "qAddModFinalizer" $
            isSuccess $ runTestQ unmockedState $ qAddModFinalizer $ return ()
          )

      -- Not testing qAddCorePlugin because I don't want to actually register plugins here

      , $(testCaseTH "qGetQ, qPutQ" $
            checkUnmockedMatches $ do
              qPutQ "Hello"
              qGetQ @_ @String
          )
      , $(testCaseTH "qIsExtEnabled" $
            checkUnmockedMatches $ sequence
              [ qIsExtEnabled JavaScriptFFI
              , qIsExtEnabled TemplateHaskell
              ]
          )
      , $(testCaseTH "qExtsEnabled" $
            checkUnmockedMatches qExtsEnabled
          )
      ]
