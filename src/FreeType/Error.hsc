{-# LANGUAGE PatternSynonyms #-}
module FreeType.Error
    ( ErrorCode
        ( ..
        , Ok
        , CannotOpenResource
        , UnknownFileFormat
        , InvalidFileFormat
        , InvalidVersion
        , LowerModuleVersion
        , InvalidArgument
        , UnimplementedFeature
        , InvalidTable
        , InvalidOffset
        , ArrayTooLarge
        , MissingModule
        , MissingProperty
        , InvalidGlyphIndex
        , InvalidCharacterCode
        , InvalidGlyphFormat
        , CannotRenderGlyph
        , InvalidOutline
        , InvalidComposite
        , TooManyHints
        , InvalidPixelSize
        , InvalidHandle
        , InvalidLibraryHandle
        , InvalidDriverHandle
        , InvalidFaceHandle
        , InvalidSizeHandle
        , InvalidSlotHandle
        , InvalidCharMapHandle
        , InvalidCacheHandle
        , InvalidStreamHandle
        , TooManyDrivers
        , TooManyExtensions
        , OutOfMemory
        , UnlistedObject
        , CannotOpenStream
        , InvalidStreamSeek
        , InvalidStreamSkip
        , InvalidStreamRead
        , InvalidStreamOperation
        , InvalidFrameOperation
        , NestedFrameAccess
        , InvalidFrameRead
        , RasterUninitialized
        , RasterCorrupted
        , RasterOverflow
        , RasterNegativeHeight
        , TooManyCaches
        , InvalidOpcode
        , TooFewArguments
        , StackOverflow
        , CodeOverflow
        , BadArgument
        , DivideByZero
        , InvalidReference
        , DebugOpCode
        , ENDFInExecStream
        , NestedDEFS
        , InvalidCodeRange
        , ExecutionTooLong
        , TooManyFunctionDefs
        , TooManyInstructionDefs
        , TableMissing
        , HorizHeaderMissing
        , LocationsMissing
        , NameTableMissing
        , CMapTableMissing
        , HmtxTableMissing
        , PostTableMissing
        , InvalidHorizMetrics
        , InvalidCharMapFormat
        , InvalidPPem
        , InvalidVertMetrics
        , CouldNotFindContext
        , InvalidPostTableFormat
        , InvalidPostTable
        , DEFInGlyfBytecode
        , MissingBitmap
        , SyntaxError
        , StackUnderflow
        , Ignore
        , NoUnicodeGlyphName
        , GlyphTooBig
        , MissingStartfontField
        , MissingFontField
        , MissingSizeField
        , MissingFontboundingboxField
        , MissingCharsField
        , MissingStartcharField
        , MissingEncodingField
        , MissingBbxField
        , BbxTooBig
        , CorruptedFontHeader
        , CorruptedFontGlyphs
        )
    , errorHandler
    , unwrapError
    , isOk, isError
    ) where

import Control.Monad (when)
import Control.Exception (ioError)
import System.IO.Error (userError)
import System.IO.Unsafe (unsafePerformIO)

import Foreign.C.Types (CLong(..))
import Foreign.C.String (CString(..), peekCString)

import Text.Printf (printf)

#include <ft2build.h>
#include FT_FREETYPE_H

-- |Error code returned from FreeType functions.
newtype ErrorCode = ErrorCode { getErrorCode :: CLong }

-- |Error handler for FreeType errors.
errorHandler :: String -> ErrorCode -> IO ()
errorHandler msg e
    = let n = fromIntegral (getErrorCode e) :: Int
    in when (n /= 0)
    $ ioError $ userError
    $ printf "FreeType Error (%d): %s\nMessage: %s" n (show e) msg

-- |Error handler wrapper for IO monads.
unwrapError :: String -> IO ErrorCode -> IO ()
unwrapError msg = (>>= errorHandler msg)

-- |Check whether an ErrorCode is not an error.
isOk :: ErrorCode -> Bool
isOk Ok = True
isOk _  = False

-- |Check whether an ErrorCode is an error.
isError :: ErrorCode -> Bool
isError = not . isOk

pattern Ok                          = ErrorCode (#const FT_Err_Ok)
pattern CannotOpenResource          = ErrorCode (#const FT_Err_Cannot_Open_Resource)
pattern UnknownFileFormat           = ErrorCode (#const FT_Err_Unknown_File_Format)
pattern InvalidFileFormat           = ErrorCode (#const FT_Err_Invalid_File_Format)
pattern InvalidVersion              = ErrorCode (#const FT_Err_Invalid_Version)
pattern LowerModuleVersion          = ErrorCode (#const FT_Err_Lower_Module_Version)
pattern InvalidArgument             = ErrorCode (#const FT_Err_Invalid_Argument)
pattern UnimplementedFeature        = ErrorCode (#const FT_Err_Unimplemented_Feature)
pattern InvalidTable                = ErrorCode (#const FT_Err_Invalid_Table)
pattern InvalidOffset               = ErrorCode (#const FT_Err_Invalid_Offset)
pattern ArrayTooLarge               = ErrorCode (#const FT_Err_Array_Too_Large)
pattern MissingModule               = ErrorCode (#const FT_Err_Missing_Module)
pattern MissingProperty             = ErrorCode (#const FT_Err_Missing_Property)
pattern InvalidGlyphIndex           = ErrorCode (#const FT_Err_Invalid_Glyph_Index)
pattern InvalidCharacterCode        = ErrorCode (#const FT_Err_Invalid_Character_Code)
pattern InvalidGlyphFormat          = ErrorCode (#const FT_Err_Invalid_Glyph_Format)
pattern CannotRenderGlyph           = ErrorCode (#const FT_Err_Cannot_Render_Glyph)
pattern InvalidOutline              = ErrorCode (#const FT_Err_Invalid_Outline)
pattern InvalidComposite            = ErrorCode (#const FT_Err_Invalid_Composite)
pattern TooManyHints                = ErrorCode (#const FT_Err_Too_Many_Hints)
pattern InvalidPixelSize            = ErrorCode (#const FT_Err_Invalid_Pixel_Size)
pattern InvalidHandle               = ErrorCode (#const FT_Err_Invalid_Handle)
pattern InvalidLibraryHandle        = ErrorCode (#const FT_Err_Invalid_Library_Handle)
pattern InvalidDriverHandle         = ErrorCode (#const FT_Err_Invalid_Driver_Handle)
pattern InvalidFaceHandle           = ErrorCode (#const FT_Err_Invalid_Face_Handle)
pattern InvalidSizeHandle           = ErrorCode (#const FT_Err_Invalid_Size_Handle)
pattern InvalidSlotHandle           = ErrorCode (#const FT_Err_Invalid_Slot_Handle)
pattern InvalidCharMapHandle        = ErrorCode (#const FT_Err_Invalid_CharMap_Handle)
pattern InvalidCacheHandle          = ErrorCode (#const FT_Err_Invalid_Cache_Handle)
pattern InvalidStreamHandle         = ErrorCode (#const FT_Err_Invalid_Stream_Handle)
pattern TooManyDrivers              = ErrorCode (#const FT_Err_Too_Many_Drivers)
pattern TooManyExtensions           = ErrorCode (#const FT_Err_Too_Many_Extensions)
pattern OutOfMemory                 = ErrorCode (#const FT_Err_Out_Of_Memory)
pattern UnlistedObject              = ErrorCode (#const FT_Err_Unlisted_Object)
pattern CannotOpenStream            = ErrorCode (#const FT_Err_Cannot_Open_Stream)
pattern InvalidStreamSeek           = ErrorCode (#const FT_Err_Invalid_Stream_Seek)
pattern InvalidStreamSkip           = ErrorCode (#const FT_Err_Invalid_Stream_Skip)
pattern InvalidStreamRead           = ErrorCode (#const FT_Err_Invalid_Stream_Read)
pattern InvalidStreamOperation      = ErrorCode (#const FT_Err_Invalid_Stream_Operation)
pattern InvalidFrameOperation       = ErrorCode (#const FT_Err_Invalid_Frame_Operation)
pattern NestedFrameAccess           = ErrorCode (#const FT_Err_Nested_Frame_Access)
pattern InvalidFrameRead            = ErrorCode (#const FT_Err_Invalid_Frame_Read)
pattern RasterUninitialized         = ErrorCode (#const FT_Err_Raster_Uninitialized)
pattern RasterCorrupted             = ErrorCode (#const FT_Err_Raster_Corrupted)
pattern RasterOverflow              = ErrorCode (#const FT_Err_Raster_Overflow)
pattern RasterNegativeHeight        = ErrorCode (#const FT_Err_Raster_Negative_Height)
pattern TooManyCaches               = ErrorCode (#const FT_Err_Too_Many_Caches)
pattern InvalidOpcode               = ErrorCode (#const FT_Err_Invalid_Opcode)
pattern TooFewArguments             = ErrorCode (#const FT_Err_Too_Few_Arguments)
pattern StackOverflow               = ErrorCode (#const FT_Err_Stack_Overflow)
pattern CodeOverflow                = ErrorCode (#const FT_Err_Code_Overflow)
pattern BadArgument                 = ErrorCode (#const FT_Err_Bad_Argument)
pattern DivideByZero                = ErrorCode (#const FT_Err_Divide_By_Zero)
pattern InvalidReference            = ErrorCode (#const FT_Err_Invalid_Reference)
pattern DebugOpCode                 = ErrorCode (#const FT_Err_Debug_OpCode)
pattern ENDFInExecStream            = ErrorCode (#const FT_Err_ENDF_In_Exec_Stream)
pattern NestedDEFS                  = ErrorCode (#const FT_Err_Nested_DEFS)
pattern InvalidCodeRange            = ErrorCode (#const FT_Err_Invalid_CodeRange)
pattern ExecutionTooLong            = ErrorCode (#const FT_Err_Execution_Too_Long)
pattern TooManyFunctionDefs         = ErrorCode (#const FT_Err_Too_Many_Function_Defs)
pattern TooManyInstructionDefs      = ErrorCode (#const FT_Err_Too_Many_Instruction_Defs)
pattern TableMissing                = ErrorCode (#const FT_Err_Table_Missing)
pattern HorizHeaderMissing          = ErrorCode (#const FT_Err_Horiz_Header_Missing)
pattern LocationsMissing            = ErrorCode (#const FT_Err_Locations_Missing)
pattern NameTableMissing            = ErrorCode (#const FT_Err_Name_Table_Missing)
pattern CMapTableMissing            = ErrorCode (#const FT_Err_CMap_Table_Missing)
pattern HmtxTableMissing            = ErrorCode (#const FT_Err_Hmtx_Table_Missing)
pattern PostTableMissing            = ErrorCode (#const FT_Err_Post_Table_Missing)
pattern InvalidHorizMetrics         = ErrorCode (#const FT_Err_Invalid_Horiz_Metrics)
pattern InvalidCharMapFormat        = ErrorCode (#const FT_Err_Invalid_CharMap_Format)
pattern InvalidPPem                 = ErrorCode (#const FT_Err_Invalid_PPem)
pattern InvalidVertMetrics          = ErrorCode (#const FT_Err_Invalid_Vert_Metrics)
pattern CouldNotFindContext         = ErrorCode (#const FT_Err_Could_Not_Find_Context)
pattern InvalidPostTableFormat      = ErrorCode (#const FT_Err_Invalid_Post_Table_Format)
pattern InvalidPostTable            = ErrorCode (#const FT_Err_Invalid_Post_Table)
pattern DEFInGlyfBytecode           = ErrorCode (#const FT_Err_DEF_In_Glyf_Bytecode)
pattern MissingBitmap               = ErrorCode (#const FT_Err_Missing_Bitmap)
pattern SyntaxError                 = ErrorCode (#const FT_Err_Syntax_Error)
pattern StackUnderflow              = ErrorCode (#const FT_Err_Stack_Underflow)
pattern Ignore                      = ErrorCode (#const FT_Err_Ignore)
pattern NoUnicodeGlyphName          = ErrorCode (#const FT_Err_No_Unicode_Glyph_Name)
pattern GlyphTooBig                 = ErrorCode (#const FT_Err_Glyph_Too_Big)
pattern MissingStartfontField       = ErrorCode (#const FT_Err_Missing_Startfont_Field)
pattern MissingFontField            = ErrorCode (#const FT_Err_Missing_Font_Field)
pattern MissingSizeField            = ErrorCode (#const FT_Err_Missing_Size_Field)
pattern MissingFontboundingboxField = ErrorCode (#const FT_Err_Missing_Fontboundingbox_Field)
pattern MissingCharsField           = ErrorCode (#const FT_Err_Missing_Chars_Field)
pattern MissingStartcharField       = ErrorCode (#const FT_Err_Missing_Startchar_Field)
pattern MissingEncodingField        = ErrorCode (#const FT_Err_Missing_Encoding_Field)
pattern MissingBbxField             = ErrorCode (#const FT_Err_Missing_Bbx_Field)
pattern BbxTooBig                   = ErrorCode (#const FT_Err_Bbx_Too_Big)
pattern CorruptedFontHeader         = ErrorCode (#const FT_Err_Corrupted_Font_Header)
pattern CorruptedFontGlyphs         = ErrorCode (#const FT_Err_Corrupted_Font_Glyphs)

instance Show ErrorCode where
    show Ok                          = "no error"
    show CannotOpenResource          = "cannot open resource"
    show UnknownFileFormat           = "unknown file format"
    show InvalidFileFormat           = "broken file"
    show InvalidVersion              = "invalid FreeType version"
    show LowerModuleVersion          = "module version is too low"
    show InvalidArgument             = "invalid argument"
    show UnimplementedFeature        = "unimplemented feature"
    show InvalidTable                = "broken table"
    show InvalidOffset               = "broken offset within table"
    show ArrayTooLarge               = "array allocation size too large"
    show MissingModule               = "missing module"
    show MissingProperty             = "missing property"
    show InvalidGlyphIndex           = "invalid glyph index"
    show InvalidCharacterCode        = "invalid character code"
    show InvalidGlyphFormat          = "unsupported glyph image format"
    show CannotRenderGlyph           = "cannot render this glyph format"
    show InvalidOutline              = "invalid outline"
    show InvalidComposite            = "invalid composite glyph"
    show TooManyHints                = "too many hints"
    show InvalidPixelSize            = "invalid pixel size"
    show InvalidHandle               = "invalid object handle"
    show InvalidLibraryHandle        = "invalid library handle"
    show InvalidDriverHandle         = "invalid module handle"
    show InvalidFaceHandle           = "invalid face handle"
    show InvalidSizeHandle           = "invalid size handle"
    show InvalidSlotHandle           = "invalid glyph slot handle"
    show InvalidCharMapHandle        = "invalid charmap handle"
    show InvalidCacheHandle          = "invalid cache manager handle"
    show InvalidStreamHandle         = "invalid stream handle"
    show TooManyDrivers              = "too many modules"
    show TooManyExtensions           = "too many extensions"
    show OutOfMemory                 = "out of memory"
    show UnlistedObject              = "unlisted object"
    show CannotOpenStream            = "cannot open stream"
    show InvalidStreamSeek           = "invalid stream seek"
    show InvalidStreamSkip           = "invalid stream skip"
    show InvalidStreamRead           = "invalid stream read"
    show InvalidStreamOperation      = "invalid stream operation"
    show InvalidFrameOperation       = "invalid frame operation"
    show NestedFrameAccess           = "nested frame access"
    show InvalidFrameRead            = "invalid frame read"
    show RasterUninitialized         = "raster uninitialized"
    show RasterCorrupted             = "raster corrupted"
    show RasterOverflow              = "raster overflow"
    show RasterNegativeHeight        = "negative height while rastering"
    show TooManyCaches               = "too many registered caches"
    show InvalidOpcode               = "invalid opcode"
    show TooFewArguments             = "too few arguments"
    show StackOverflow               = "stack overflow"
    show CodeOverflow                = "code overflow"
    show BadArgument                 = "bad argument"
    show DivideByZero                = "division by zero"
    show InvalidReference            = "invalid reference"
    show DebugOpCode                 = "found debug opcode"
    show ENDFInExecStream            = "found ENDF opcode in execution stream"
    show NestedDEFS                  = "nested DEFS"
    show InvalidCodeRange            = "invalid code range"
    show ExecutionTooLong            = "execution context too long"
    show TooManyFunctionDefs         = "too many function definitions"
    show TooManyInstructionDefs      = "too many instruction definitions"
    show TableMissing                = "SFNT font table missing"
    show HorizHeaderMissing          = "horizontal header (hhea) table missing"
    show LocationsMissing            = "locations (loca) table missing"
    show NameTableMissing            = "name table missing"
    show CMapTableMissing            = "character map (cmap) table missing"
    show HmtxTableMissing            = "horizontal metrics (hmtx) table missing"
    show PostTableMissing            = "PostScript (post) table missing"
    show InvalidHorizMetrics         = "invalid horizontal metrics"
    show InvalidCharMapFormat        = "invalid character map (cmap) format"
    show InvalidPPem                 = "invalid ppem value"
    show InvalidVertMetrics          = "invalid vertical metrics"
    show CouldNotFindContext         = "could not find context"
    show InvalidPostTableFormat      = "invalid PostScript (post) table format"
    show InvalidPostTable            = "invalid PostScript (post) table"
    show DEFInGlyfBytecode           = "found FDEF or IDEF opcode in glyf bytecode"
    show MissingBitmap               = "missing bitmap in strike"
    show SyntaxError                 = "opcode syntax error"
    show StackUnderflow              = "argument stack underflow"
    show Ignore                      = "ignore"
    show NoUnicodeGlyphName          = "no Unicode glyph name found"
    show GlyphTooBig                 = "glyph too big for hinting"
    show MissingStartfontField       = "`STARTFONT' field missing"
    show MissingFontField            = "`FONT' field missing"
    show MissingSizeField            = "`SIZE' field missing"
    show MissingFontboundingboxField = "`FONTBOUNDINGBOX' field missing"
    show MissingCharsField           = "`CHARS' field missing"
    show MissingStartcharField       = "`STARTCHAR' field missing"
    show MissingEncodingField        = "`ENCODING' field missing"
    show MissingBbxField             = "`BBX' field missing"
    show BbxTooBig                   = "`BBX' too big"
    show CorruptedFontHeader         = "Font header corrupted or missing fields"
    show CorruptedFontGlyphs         = "Font glyphs corrupted or missing fields"
    show _                           = "Unknown FreeType error"
