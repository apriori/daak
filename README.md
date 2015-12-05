daak
====================
Source code of my diploma thesis dealing with a genetic algorithm for a packing problem. A given packing problem is solved by optimization of multiple criteria simultaneously using NSGA-II.

![ScreenShot](https://github.com/apriori/daak/blob/master/example_het1.png)

# Prerequisites
Your environment needs the package _HTF_ (installed OUTSIDE of a sandbox. It needs be available to the execution environment of ghc). This package was tested with ghc 7.10.2. Available platforms are constrainted by the availability of an implemention of OpenGL for it. You also need _cabal-meta_.

# Buid/Install
- Start by creating a sandbox somewhere, e.g. directly in the repository root by
```
cabal sandbox init
```
  (alternatively create it somewhere else and make sure the variabe CABAL_SANBOX_CONFIG is set.
- Run the following in the root directory of the repository.
```
cabal-meta install 
```
This will fetch, build and install all dependencies and the resulting program _livealgo_. _haskell-monad-mersenne-random_ and _moo_ will be checked out in /vendor and built prior to _daak_.

# Execute
To execute the algorithm on the test samples in /testdata, use e.g.

```
cabal run ./testdata/het_1.json
```

and enjoy.

# Further info
By default the algorithm will update the generation every 0.35 s and rerender the current worst and best solution defined by the multiple objective problem which is specified in https://github.com/apriori/daak/blob/master/src/LiveAlgo.hs#L251

## Controls for the visualisation
- Mouse move + Left click : Rotate around focus point
- Mouse move + right click : Move in current direction parallel to the floor 
- Mousewheel up/down: Zoom in/out

Please zoom out first and rotate the camera, since an initial camera setup isn't done, yet.



## File input format.
The JSON format itself should be self-explanatory. A problem consists of a single load specification (with loadSpaceSize being an array [l, w, h] for the size) and an array of items, each consisting of:

- itemName :: String
- quantity :: Number
- itemSize :: [l, w, h]

## Note
By default this application will use all your cores. The cpu load is highly dependant on the packing problem size, though. 

## License note
Before deriving any work from this source code make sure to read the [License](https://github.com/apriori/daak/blob/master/LICENSE) and [LicenseNSGA2](https://github.com/apriori/daak/blob/master/LicenseNSGA2).

