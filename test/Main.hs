{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.List (intercalate)
import Language.Haskell.TH (AnnLookup(..), Q, Type(..))
import Language.Haskell.TH.Syntax (Extension(..), Quasi(..))
#if MIN_VERSION_template_haskell(2,12,0)
import Language.Haskell.TH.Syntax (ForeignSrcLang(..))
#endif
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Language.Haskell.TH.TestUtils
import TestLib
import TH

main :: IO ()
main = defaultMain $ testGroup "th-test-utils"
  [ testAllowQ
  , testMockQ
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

testMockQ :: TestTree
testMockQ = testGroup "MockQ"
  [ testRunners
  , testMethods
  ]
  where
    mockedState = QState
      { mode = MockQ
      , knownNames = []
      , reifyInfo = []
      }

    -- Call runTestQ, ensuring that the result is evaluated
    runTestQ' :: QState 'FullyMocked -> Q a -> IO a
    runTestQ' state = forceM . pure . runTestQ state

    runTestQWithErrors :: QState 'FullyMocked -> Q a -> IO (Either String a)
    runTestQWithErrors state = tryIO . runTestQ' state

    testRunners = testGroup "tryTestQ, runTestQ, runTestQErr"
      [ testCase "tryTestQ - success" $ do
          let x = 1
              result = tryTestQ mockedState (return x :: Q Int)
          result @?= Right x
      , testCase "tryTestQ - error" $ do
          let msg = "Error message"
              result = tryTestQ mockedState (fail msg :: Q Int)
          result @?= Left msg
      , testCase "Right . runTestQ === tryTestQ" $ do
          let actual = Right $ runTestQ mockedState basicSuccess
              expected = tryTestQ mockedState basicSuccess
          actual @?= expected
      , testCase "runTestQ errors on failure" $ do
          let msg = "Error message"
          result <- tryIO $ forceM $ pure $ runTestQ mockedState (fail msg :: Q ())
          result @?= Left msg
      , testCase "Left . runTestQErr === tryTestQ" $ do
          let actual = Left $ runTestQErr mockedState basicFailure
              expected = tryTestQ mockedState basicFailure
          actual @?= expected
      , testCase "runTestQErr errors on success" $ do
          result <- tryIO $ forceM $ pure $ runTestQErr mockedState (return 1 :: Q Int)
          result @?= Left "Unexpected success: 1"
      ]

    testMethods = testGroup "Quasi methods"
      [ golden "qNewName" $
          labelled $ runTestQ mockedState $ do
            name1 <- qNewName "foo"
            name2 <- qNewName "foo"
            name3 <- qNewName "bar"
            return
              [ ("Name 1", pure name1)
              , ("Name 2", pure name2)
              , ("Name 3", pure name3)
              ]
      , testCase "qReport" $ do
          let warningResult = runTestQ mockedState $ qReport False "A warning message"
          warningResult @?= ()

          let errorResult = runTestQ mockedState $ qReport True "An error message"
          errorResult @?= ()
      , testCase "qRecover" $ do
          let x = "Success"
              result = runTestQ mockedState $ qRecover (return x) basicFailure
          result @?= x
      , testCase "qLookupName" $ do
          let state = mockedState { knownNames = [("Show", ''Show), ("Right", 'Right)] }
          runTestQ state (qLookupName True "Show") @?= Just ''Show
          runTestQ state (qLookupName True "Eq") @?= Nothing
          runTestQ state (qLookupName False "Right") @?= Just 'Right
          runTestQ state (qLookupName False "Left") @?= Nothing
      , golden "qReify" $ do
          let state = mockedState { reifyInfo = $(loadNames ['putStrLn]) }
          labelled
            [ ("Found", runTestQWithErrors state $ qReify 'putStrLn)
            , ("Missing", runTestQWithErrors state $ qReify ''Show)
            ]
      , golden "qReifyFixity" $ do
          let state = mockedState { reifyInfo = $(loadNames ['($)]) }
          labelled
            [ ("Found", runTestQWithErrors state $ qReifyFixity '($))
            , ("Missing", runTestQWithErrors state $ qReifyFixity '(+))
            ]
#if MIN_VERSION_template_haskell(2,16,0)
      , golden "qReifyType" $ do
          let state = mockedState { reifyInfo = $(loadNames [''Maybe]) }
          labelled
            [ ("Found", runTestQWithErrors state $ qReifyType ''Maybe)
            , ("Missing", runTestQWithErrors state $ qReifyType ''Show)
            ]
#endif
      , golden "qReifyInstances" $
          runUnsupported mockedState $ qReifyInstances ''Show [ConT ''Int]
      , golden "qReifyRoles" $ do
          let state = mockedState { reifyInfo = $(loadNames [''Maybe, 'map]) }
          labelled
            [ ("Found", runTestQWithErrors state $ qReifyRoles ''Maybe)
              -- reifyRoles errors if name is not of a type constructor
            , ("Invalid", runTestQWithErrors state $ qReifyRoles 'map)
            , ("Missing", runTestQWithErrors state $ qReifyRoles ''Show)
            ]
      , golden "qReifyAnnotations" $
          runUnsupported mockedState $ qReifyAnnotations @_ @[String] (AnnLookupName 'basicSuccess)
      , golden "qReifyModule" $
          runUnsupported mockedState $ qReifyModule $(thisModule)
      , golden "qReifyConStrictness" $
          runUnsupported mockedState $ qReifyConStrictness 'Just
      , golden "qLocation" $
          runUnsupported mockedState qLocation
      , golden "qRunIO" $
          labelled
            [ ("Disallowed", runTestQWithErrors mockedState $ qRunIO $ putStrLn "Hello world")
            ]
      , golden "qAddDependentFile" $
          runUnsupported mockedState $ qAddDependentFile "README.md"
      , golden "qAddTopDecls" $
          runUnsupported mockedState $ qAddTopDecls []
#if MIN_VERSION_template_haskell(2,14,0)
      , golden "qAddTempFile" $
          runUnsupported mockedState $ qAddTempFile "c"
      , golden "qAddForeignFilePath" $
          runUnsupported mockedState $ qAddForeignFilePath LangC "foo.c"
#elif MIN_VERSION_template_haskell(2,12,0)
      , golden "qAddForeignFile" $
          runUnsupported mockedState $ qAddForeignFile LangC "#include <stdio.h>"
#endif
      , golden "qAddModFinalizer" $
          runUnsupported mockedState $ qAddModFinalizer $ return ()
#if MIN_VERSION_template_haskell(2,13,0)
      , golden "qAddCorePlugin" $
          runUnsupported mockedState $ qAddCorePlugin "MyPlugin"
#endif
      , golden "qGetQ" $
          runUnsupported mockedState $ qGetQ @_ @String
      , golden "qPutQ" $
          runUnsupported mockedState $ qPutQ "Hello"
      , golden "qIsExtEnabled" $
          runUnsupported mockedState $ qIsExtEnabled TemplateHaskell
      , golden "qExtsEnabled" $
          runUnsupported mockedState qExtsEnabled
      ]

    golden name = goldenVsString name ("test/goldens/MockQ_" ++ name ++ ".golden")

    labelled :: Show a => [(String, IO a)] -> IO ByteString
    labelled vals = do
      let mkLine (label, getVal) = do
            val <- getVal
            return $ label ++ ": " ++ show val
      Char8.pack . intercalate "\n" . map (++ "\n") <$> mapM mkLine vals

    runUnsupported :: Show a => QState 'FullyMocked -> Q a -> IO ByteString
    runUnsupported state q = labelled [("Unsupported", runTestQWithErrors state q)]
