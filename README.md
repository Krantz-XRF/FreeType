# Haskell Bindings for `FreeType`

Copyright Krantz-XRF (C) 2019

## Description

What is this project aimed for? It is mainly for handling the outline glyphs in font files. Other support are planned, but not in the highest priority.

## Low- and High-level API: `FreeType`

See [`FreeType`](FreeType). See also [the online Haddock documentation](https://krantz-xrf.github.io/FreeType/).

## Rendering Backend: `FreeType-Rasterific`

See [`FreeType-Rasterific`](FreeType-Rasterific). See also [the online Haddock documentation](https://krantz-xrf.github.io/FreeType/).

## Illustration: `FreeType-convert`

See [`FreeType-convert`](FreeType-convert). See also [the online Haddock documentation](https://krantz-xrf.github.io/FreeType/).

## The `HarfBuzz` Binding

**Warning**: This binding is automatically generated from the HarfBuzz's C headers. It is currently not well tested, so use at your own risk.

See [`HarfBuzz`](HarfBuzz). Documentation is not available at the moment.

## Reference

Documentation can be generated using Haddock. Online documentation can be found at [My Blog](https://krantz-xrf.github.io/FreeType/). Please note that this documentation may not be up to date with the master branch.

For the data types (low level or high level), please refer to `FreeType.LowLevel.Types`, `FreeType.Types`, `FreeType.LowLevel.Size`, `FreeType.LowLevel.Generic`, and `Data.FixedPoint`.

For the bindings, please refer to the corresponding module, e.g. for APIs concerning `FT_Glyph`, first remove the `FT_` prefix to get the Haskell type `Glyph`, and refer to module `FreeType.LowLevel.Glyph` and `FreeType.Glyph`.

For rendering, please refer to `FreeType.Rasterific`.
