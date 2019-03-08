# template-haskell-test-utils

This package contains utility functions for testing Template Haskell code.

## Usage

TODO

## Developing

### Build

```bash
scripts/install-stack-deps.sh
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
