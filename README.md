# th-test-utils

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/th-test-utils.svg)
![Hackage](https://img.shields.io/hackage/v/th-test-utils.svg)
[![codecov](https://codecov.io/gh/LeapYear/th-test-utils/branch/master/graph/badge.svg)](https://codecov.io/gh/LeapYear/th-test-utils)

This package implements `tryTestQ` and related helpers in order to better test Template Haskell code. It supports returning the actual error message that [`recover` doesn't currently return](https://gitlab.haskell.org/ghc/ghc/-/issues/2340) as well as mocking out `Q` actions, so that you can run Template Haskell code at runtime.

## Usage

```haskell
-- e.g. $(showInfo "Bool") generates a string corresponding
-- to the reify `Info` for `Bool`.
showInfo :: String -> Q Exp
showInfo s = do
  mName <- lookupTypeName s
  case mName of
    Nothing -> fail $ "Unknown type: " ++ s
    Just name -> do
      info <- reify name
      lift $ show info
```

```haskell
-- example using tasty-hunit
main :: IO ()
main = defaultMain $ testGroup "my-project"
  [ testCase "showInfo unmocked" $(do
      result1 <- tryTestQ unmockedState $ showInfo "Bool"
      runIO $ isRight result1 @? ("Unexpected error: " ++ show result1)

      result2 <- tryTestQ unmockedState $ showInfo "Foo"
      runIO $ result2 @?= Left "Unknown type: Foo"

      [| return () |]
    )

  , testCase "showInfo mocked success" $ do
      let state = QState
            { mode = MockQ
            , knownNames = [("Bool", ''Bool)]
            , reifyInfo = $(loadNames [''Bool])
            }

      let result1 = tryTestQ state $ showInfo "Bool"
      isRight result1 @? ("Unexpected error: " ++ show result1)

      let result2 = tryTestQ state $ showInfo "Foo"
      result2 @?= Left "Unknown type: Foo"
  ]
```
