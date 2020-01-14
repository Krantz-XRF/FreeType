# Haskell Bindings for `FreeType`

## Low- and High-level API: `FreeType`

See [`FreeType`](FreeType).

## Rendering Backend: `FreeType-Rasterific`

See [`FreeType-Rasterific`](FreeType-Rasterific).

## Illustration: `FreeType-convert`

See [`FreeType-convert`](FreeType-convert).

## Reference

Documentation can be generated using Haddock.

For the data types (low level or high level), please refer to `FreeType.LowLevel.Types`, `FreeType.Types`, `FreeType.LowLevel.Size`, `FreeType.LowLevel.Generic`, and `Data.FixedPoint`.

For the bindings, please refer to the corresponding module, e.g. for APIs concerning `FT_Glyph`, first remove the `FT_` prefix to get the Haskell type `Glyph`, and refer to module `FreeType.LowLevel.Glyph` and `FreeType.Glyph`.
