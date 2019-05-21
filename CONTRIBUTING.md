# Guide to contribution

## Quickstart

### Build

Builds must pass with Haddock enabled in order for your patch to be accepted.

```bash
stack build

# with haddock
stack build --haddock
```

### Lint

The following linters must pass CI in order for your patch to be accepted.

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

All tests must pass CI in order for your patch to be accepted.

```bash
stack test
```

## Documentation

All code should be fully documented, whether adding comments for future
developers or adding Haddock docs for functionality exposed in Haddock docs.

Changes that affect users should be mentioned in `CHANGELOG.md`.

Add an entry under under the `Upcoming` header containing:
    * A description of the change
    * The type of change (breaking, bugfix, etc.)
    * If applicable,
        * How to migrate existing code
        * When it should be used
        * What it supersedes

The format is not important, as the list will be curated when releasing.
