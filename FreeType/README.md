# FreeType

(Partial) low level and high level bindings for FreeType.

## Description

What is this project aimed for? It is mainly for handling the outline glyphs in font files. Other support are planned, but not in the highest priority.

## Basic Usage

Some important API are listed as follows:

Use `withFreeType` to wrap your `main` function:

```haskell
FreeType.Library.withFreeType
    :: (Library -> IO a)    -- the procedure
    -> IO a     -- automatically load and free the library
```

Use `withFace` to load a font face:

```haskell
FreeType.Face.withFace
    :: Library          -- the loaded library
    -> String           -- the file path for the font face
    -> Int              -- the index for the font face
    -> (Face -> IO a)   -- the procedure
    -> IO a             -- automatically load and discard
```

Use `withCharGlyph` to load a character glyph:

```haskell
FreeType.Glyph.withCharGlyph
    :: Face             -- the loaded font face
    -> Char             -- the character to load
    -> [LoadFlags]      -- load flags
    -> (Glyph -> IO b)  -- the procedure
    -> IO b             -- automatically load and discard
```

APIs in `FreeType.Outline` are useful to handle glyph outlines:

```haskell
FreeType.Outline.printOutlineSVG
    :: String   -- colour string
    -> Handle   -- the file handle to print to
    -> POutline -- the pointer to the outline
    -> IO ()    -- print in SVG format to the file
```

```haskell
FreeType.Outline.extractBezier
    :: Bezier b -- see the following class Bezier
    => POutline -- the pointer to the outline
    -> Int      -- shift
    -> Int      -- delta
    -> IO b     -- the extracted Bezier curves
```

Note: what is `shift` ans `delta`? The coordinates of the Bezier curves are transformed as if passed to ``\x -> x `shiftL` shift - delta``.

Bezier curves should implement `class Bezier`:

```haskell
class Bezier b where
    emptyBezier :: b
    moveTo :: Vector Int -> b -> b
    lineTo :: Vector Int -> b -> b
    conicTo :: Vector Int -> Vector Int -> b -> b
    cubicTo :: Vector Int -> Vector Int -> Vector Int -> b -> b
```

Or, if the Bezier curve is represented as a list of Bezier curve segments, the segment type should implement `class BezierSegment`:

```haskell
class BezierSegment b where
    lineFromTo :: Vector Int -> Vector Int -> b
    conicFromTo :: Vector Int -> Vector Int -> Vector Int -> b
    cubicFromTo :: Vector Int -> Vector Int -> Vector Int -> Vector Int -> b
```

If any type `b` is an instance of `BezierSegment`, `[b]` is automatically implemented as `Bezier` instances. So choose to implement either one is OK.
