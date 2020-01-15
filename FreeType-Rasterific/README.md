# FreeType-Rasterific

Support for rendering texts with the `FreeType` library to a `Rasterific` image.

## Basic Usage

Use `renderChar` and `renderCharAt` to get a `TextOutline` for a character:

```haskell
renderChar :: Face -> Char -> IO TextOutline
renderCharAt :: Face -> Char -> Point -> IO TextOutline
```

`TextOutline` has a `Geometry` instance, so can be further rendered by `Graphics.Rasterific.renderDrawing` and then by `Codec.Picture.writePng` et al.

Use `translate` to translate a `TextOutline`. After translation, the origin will land on the given point:

```haskell
translate :: Point -> TextOutline -> TextOutline
```
