# th-test-utils

This package contains utility functions for testing Template Haskell code.

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
  , testCase "numberify" $
      $(tryQ $ quoteExp numberify "one")   @?= (Right 1 :: Either String Int)
  , testCase "numberify" $
      $(tryQ $ quoteExp numberify "foo")   @?= (Left "not a number" :: Either String Int)
  ]
```

## Developing

### Build

```bash
stack build
```

### Lint

* HLint

    ```bash
    scripts/hlint.sh
    ```

* stylish-haskell

    ```bash
    scripts/stylish-haskell.sh

    # apply stylish-haskell changes
    scripts/stylish-haskell.sh --apply
    ```

### Run tests

```bash
stack test
```
