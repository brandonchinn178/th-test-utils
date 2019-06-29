# th-test-utils

![CircleCI](https://img.shields.io/circleci/build/github/LeapYear/th-test-utils.svg)
![Hackage](https://img.shields.io/hackage/v/th-test-utils.svg)

This package contains utility functions for testing Template Haskell code.

Currently, this package only exposes a single function, `tryQ` (and derivatives),
that allows testing whether a given Template Haskell expression fails.

## Usage

```haskell
-- e.g. $(qConcat ["hello", "world"]) generates "helloworld" at compile time
qConcat :: [String] -> Q Exp
qConcat [] = fail "Cannot concat empty list"
qConcat xs = ...

-- e.g. [numberify| one |] generates `1` at compile time
numberify :: QuasiQuoter
numberify = ...
```

```haskell
-- example using tasty-hunit
main :: IO ()
main = defaultMain $ testGroup "my-project"
  [ testCase "qConcat 1" $
      $(tryQ $ qConcat ["hello", "world"]) @?= (Right "helloworld" :: Either String String)
  , testCase "qConcat 2" $
      $(tryQ $ qConcat [])                 @?= (Left "Cannot concat empty list" :: Either String String)
  , testCase "numberify 1" $
      $(tryQ $ quoteExp numberify "one")   @?= (Right 1 :: Either String Int)
  , testCase "numberify 2" $
      $(tryQ $ quoteExp numberify "foo")   @?= (Left "not a number" :: Either String Int)

  -- can also return error message as `Maybe String` or `String` (which errors
  -- if the function doesn't error)
  , testCase "numberify 3" $
      $(tryQErr $ quoteExp numberify "foo") @?= Just "not a number"
  , testCase "numberify 4" $
      $(tryQErr' $ quoteExp numberify "foo") @?= "not a number"
  ]
```
