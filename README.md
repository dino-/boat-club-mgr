# watereeboatclub


## Synopsis

Wateree Boat Club website


## Description

Reimplementation of the Wateree Boat Club website with Haskell, Yesod and SQLite


## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically be recompiled and
redeployed to localhost.


### Tests

```
stack test --flag watereeboatclub:library-only --flag watereeboatclub:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching
those flags means you don't need to recompile between tests and development,
and it disables optimization to speed up your test compile times).


## Contact

Dino Morelli <dino@ui3.info>
