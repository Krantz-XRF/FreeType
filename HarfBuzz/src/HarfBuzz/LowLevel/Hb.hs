{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module HarfBuzz.LowLevel.Hb where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc

data HbVarInt

data Blob

newtype BufferClusterLevel = BufferClusterLevelT{ unwrapBufferClusterLevel :: CInt }
pattern BufferClusterLevelMonotoneGraphemes = BufferClusterLevelT 0
pattern BufferClusterLevelMonotoneCharacters = BufferClusterLevelT 1
pattern BufferClusterLevelCharacters = BufferClusterLevelT 2
pattern BufferClusterLevelDefault = BufferClusterLevelT 0

newtype BufferContentType = BufferContentTypeT{ unwrapBufferContentType :: CInt }
pattern BufferContentTypeInvalid = BufferContentTypeT 0
pattern BufferContentTypeUnicode = BufferContentTypeT 1
pattern BufferContentTypeGlyphs = BufferContentTypeT 2

newtype BufferDiffFlags = BufferDiffFlagsT{ unwrapBufferDiffFlags :: CInt }
pattern BufferDiffFlagEqual = BufferDiffFlagsT 0
pattern BufferDiffFlagContentTypeMismatch = BufferDiffFlagsT 1
pattern BufferDiffFlagLengthMismatch = BufferDiffFlagsT 2
pattern BufferDiffFlagNotdefPresent = BufferDiffFlagsT 4
pattern BufferDiffFlagDottedCirclePresent = BufferDiffFlagsT 8
pattern BufferDiffFlagCodepointMismatch = BufferDiffFlagsT 16
pattern BufferDiffFlagClusterMismatch = BufferDiffFlagsT 32
pattern BufferDiffFlagGlyphFlagsMismatch = BufferDiffFlagsT 64
pattern BufferDiffFlagPositionMismatch = BufferDiffFlagsT 128

newtype BufferFlags = BufferFlagsT{ unwrapBufferFlags :: CInt }
pattern BufferFlagDefault = BufferFlagsT 0
pattern BufferFlagBot = BufferFlagsT 1
pattern BufferFlagEot = BufferFlagsT 2
pattern BufferFlagPreserveDefaultIgnorables = BufferFlagsT 4
pattern BufferFlagRemoveDefaultIgnorables = BufferFlagsT 8
pattern BufferFlagDoNotInsertDottedCircle = BufferFlagsT 16

newtype BufferSerializeFlags = BufferSerializeFlagsT{ unwrapBufferSerializeFlags :: CInt }
pattern BufferSerializeFlagDefault = BufferSerializeFlagsT 0
pattern BufferSerializeFlagNoClusters = BufferSerializeFlagsT 1
pattern BufferSerializeFlagNoPositions = BufferSerializeFlagsT 2
pattern BufferSerializeFlagNoGlyphNames = BufferSerializeFlagsT 4
pattern BufferSerializeFlagGlyphExtents = BufferSerializeFlagsT 8
pattern BufferSerializeFlagGlyphFlags = BufferSerializeFlagsT 16
pattern BufferSerializeFlagNoAdvances = BufferSerializeFlagsT 32

newtype BufferSerializeFormat = BufferSerializeFormatT{ unwrapBufferSerializeFormat :: CInt }
pattern BufferSerializeFormatText = BufferSerializeFormatT 1413830740
pattern BufferSerializeFormatJson = BufferSerializeFormatT 1246973774
pattern BufferSerializeFormatInvalid = BufferSerializeFormatT 0

data Buffer

newtype Direction = DirectionT{ unwrapDirection :: CInt }
pattern DirectionInvalid = DirectionT 0
pattern DirectionLtr = DirectionT 4
pattern DirectionRtl = DirectionT 5
pattern DirectionTtb = DirectionT 6
pattern DirectionBtt = DirectionT 7

data Face

data Feature

data FontExtents

data FontFuncs

data Font

data GlyphExtents

newtype GlyphFlags = GlyphFlagsT{ unwrapGlyphFlags :: CInt }
pattern GlyphFlagUnsafeToBreak = GlyphFlagsT 1
pattern GlyphFlagDefined = GlyphFlagsT 1

data GlyphInfo

data GlyphPosition

data LanguageImpl

data Map

newtype MemoryMode = MemoryModeT{ unwrapMemoryMode :: CInt }
pattern MemoryModeDuplicate = MemoryModeT 0
pattern MemoryModeReadonly = MemoryModeT 1
pattern MemoryModeWritable = MemoryModeT 2
pattern MemoryModeReadonlyMayMakeWritable = MemoryModeT 3

newtype Script = ScriptT{ unwrapScript :: CInt }
pattern ScriptCommon = ScriptT 1517910393
pattern ScriptInherited = ScriptT 1516858984
pattern ScriptUnknown = ScriptT 1517976186
pattern ScriptArabic = ScriptT 1098015074
pattern ScriptArmenian = ScriptT 1098018158
pattern ScriptBengali = ScriptT 1113943655
pattern ScriptCyrillic = ScriptT 1132032620
pattern ScriptDevanagari = ScriptT 1147500129
pattern ScriptGeorgian = ScriptT 1197830002
pattern ScriptGreek = ScriptT 1198679403
pattern ScriptGujarati = ScriptT 1198877298
pattern ScriptGurmukhi = ScriptT 1198879349
pattern ScriptHangul = ScriptT 1214344807
pattern ScriptHan = ScriptT 1214344809
pattern ScriptHebrew = ScriptT 1214603890
pattern ScriptHiragana = ScriptT 1214870113
pattern ScriptKannada = ScriptT 1265525857
pattern ScriptKatakana = ScriptT 1264676449
pattern ScriptLao = ScriptT 1281453935
pattern ScriptLatin = ScriptT 1281455214
pattern ScriptMalayalam = ScriptT 1298954605
pattern ScriptOriya = ScriptT 1332902241
pattern ScriptTamil = ScriptT 1415671148
pattern ScriptTelugu = ScriptT 1415933045
pattern ScriptThai = ScriptT 1416126825
pattern ScriptTibetan = ScriptT 1416192628
pattern ScriptBopomofo = ScriptT 1114599535
pattern ScriptBraille = ScriptT 1114792297
pattern ScriptCanadianSyllabics = ScriptT 1130458739
pattern ScriptCherokee = ScriptT 1130915186
pattern ScriptEthiopic = ScriptT 1165256809
pattern ScriptKhmer = ScriptT 1265134962
pattern ScriptMongolian = ScriptT 1299148391
pattern ScriptMyanmar = ScriptT 1299803506
pattern ScriptOgham = ScriptT 1332175213
pattern ScriptRunic = ScriptT 1383427698
pattern ScriptSinhala = ScriptT 1399418472
pattern ScriptSyriac = ScriptT 1400468067
pattern ScriptThaana = ScriptT 1416126817
pattern ScriptYi = ScriptT 1500080489
pattern ScriptDeseret = ScriptT 1148416628
pattern ScriptGothic = ScriptT 1198486632
pattern ScriptOldItalic = ScriptT 1232363884
pattern ScriptBuhid = ScriptT 1114990692
pattern ScriptHanunoo = ScriptT 1214344815
pattern ScriptTagalog = ScriptT 1416064103
pattern ScriptTagbanwa = ScriptT 1415669602
pattern ScriptCypriot = ScriptT 1131442804
pattern ScriptLimbu = ScriptT 1281977698
pattern ScriptLinearB = ScriptT 1281977954
pattern ScriptOsmanya = ScriptT 1332964705
pattern ScriptShavian = ScriptT 1399349623
pattern ScriptTaiLe = ScriptT 1415670885
pattern ScriptUgaritic = ScriptT 1432838514
pattern ScriptBuginese = ScriptT 1114990441
pattern ScriptCoptic = ScriptT 1131376756
pattern ScriptGlagolitic = ScriptT 1198285159
pattern ScriptKharoshthi = ScriptT 1265131890
pattern ScriptNewTaiLue = ScriptT 1415670901
pattern ScriptOldPersian = ScriptT 1483761007
pattern ScriptSylotiNagri = ScriptT 1400466543
pattern ScriptTifinagh = ScriptT 1415999079
pattern ScriptBalinese = ScriptT 1113681001
pattern ScriptCuneiform = ScriptT 1483961720
pattern ScriptNko = ScriptT 1315663727
pattern ScriptPhagsPa = ScriptT 1349017959
pattern ScriptPhoenician = ScriptT 1349021304
pattern ScriptCarian = ScriptT 1130459753
pattern ScriptCham = ScriptT 1130914157
pattern ScriptKayahLi = ScriptT 1264675945
pattern ScriptLepcha = ScriptT 1281716323
pattern ScriptLycian = ScriptT 1283023721
pattern ScriptLydian = ScriptT 1283023977
pattern ScriptOlChiki = ScriptT 1332503403
pattern ScriptRejang = ScriptT 1382706791
pattern ScriptSaurashtra = ScriptT 1398895986
pattern ScriptSundanese = ScriptT 1400204900
pattern ScriptVai = ScriptT 1449224553
pattern ScriptAvestan = ScriptT 1098281844
pattern ScriptBamum = ScriptT 1113681269
pattern ScriptEgyptianHieroglyphs = ScriptT 1164409200
pattern ScriptImperialAramaic = ScriptT 1098018153
pattern ScriptInscriptionalPahlavi = ScriptT 1349020777
pattern ScriptInscriptionalParthian = ScriptT 1349678185
pattern ScriptJavanese = ScriptT 1247901281
pattern ScriptKaithi = ScriptT 1265920105
pattern ScriptLisu = ScriptT 1281979253
pattern ScriptMeeteiMayek = ScriptT 1299473769
pattern ScriptOldSouthArabian = ScriptT 1398895202
pattern ScriptOldTurkic = ScriptT 1332898664
pattern ScriptSamaritan = ScriptT 1398893938
pattern ScriptTaiTham = ScriptT 1281453665
pattern ScriptTaiViet = ScriptT 1415673460
pattern ScriptBatak = ScriptT 1113683051
pattern ScriptBrahmi = ScriptT 1114792296
pattern ScriptMandaic = ScriptT 1298230884
pattern ScriptChakma = ScriptT 1130457965
pattern ScriptMeroiticCursive = ScriptT 1298494051
pattern ScriptMeroiticHieroglyphs = ScriptT 1298494063
pattern ScriptMiao = ScriptT 1349284452
pattern ScriptSharada = ScriptT 1399353956
pattern ScriptSoraSompeng = ScriptT 1399812705
pattern ScriptTakri = ScriptT 1415670642
pattern ScriptBassaVah = ScriptT 1113682803
pattern ScriptCaucasianAlbanian = ScriptT 1097295970
pattern ScriptDuployan = ScriptT 1148547180
pattern ScriptElbasan = ScriptT 1164730977
pattern ScriptGrantha = ScriptT 1198678382
pattern ScriptKhojki = ScriptT 1265135466
pattern ScriptKhudawadi = ScriptT 1399418468
pattern ScriptLinearA = ScriptT 1281977953
pattern ScriptMahajani = ScriptT 1298229354
pattern ScriptManichaean = ScriptT 1298230889
pattern ScriptMendeKikakui = ScriptT 1298493028
pattern ScriptModi = ScriptT 1299145833
pattern ScriptMro = ScriptT 1299345263
pattern ScriptNabataean = ScriptT 1315070324
pattern ScriptOldNorthArabian = ScriptT 1315009122
pattern ScriptOldPermic = ScriptT 1348825709
pattern ScriptPahawhHmong = ScriptT 1215131239
pattern ScriptPalmyrene = ScriptT 1348562029
pattern ScriptPauCinHau = ScriptT 1348564323
pattern ScriptPsalterPahlavi = ScriptT 1349020784
pattern ScriptSiddham = ScriptT 1399415908
pattern ScriptTirhuta = ScriptT 1416196712
pattern ScriptWarangCiti = ScriptT 1466004065
pattern ScriptAhom = ScriptT 1097363309
pattern ScriptAnatolianHieroglyphs = ScriptT 1215067511
pattern ScriptHatran = ScriptT 1214346354
pattern ScriptMultani = ScriptT 1299541108
pattern ScriptOldHungarian = ScriptT 1215655527
pattern ScriptSignwriting = ScriptT 1399287415
pattern ScriptAdlam = ScriptT 1097100397
pattern ScriptBhaiksuki = ScriptT 1114139507
pattern ScriptMarchen = ScriptT 1298231907
pattern ScriptOsage = ScriptT 1332963173
pattern ScriptTangut = ScriptT 1415671399
pattern ScriptNewa = ScriptT 1315272545
pattern ScriptMasaramGondi = ScriptT 1198485101
pattern ScriptNushu = ScriptT 1316186229
pattern ScriptSoyombo = ScriptT 1399814511
pattern ScriptZanabazarSquare = ScriptT 1516334690
pattern ScriptDogra = ScriptT 1148151666
pattern ScriptGunjalaGondi = ScriptT 1198485095
pattern ScriptHanifiRohingya = ScriptT 1383032935
pattern ScriptMakasar = ScriptT 1298230113
pattern ScriptMedefaidrin = ScriptT 1298490470
pattern ScriptOldSogdian = ScriptT 1399809903
pattern ScriptSogdian = ScriptT 1399809892
pattern ScriptElymaic = ScriptT 1164736877
pattern ScriptNandinagari = ScriptT 1315008100
pattern ScriptNyiakengPuachueHmong = ScriptT 1215131248
pattern ScriptWancho = ScriptT 1466132591
pattern ScriptInvalid = ScriptT 0
pattern HbScriptMaxValue = ScriptT 2147483647
pattern HbScriptMaxValueSigned = ScriptT 2147483647

data SegmentProperties

data Set

data ShapePlan

newtype UnicodeCombiningClass = UnicodeCombiningClassT{ unwrapUnicodeCombiningClass :: CInt }
pattern UnicodeCombiningClassNotReordered = UnicodeCombiningClassT 0
pattern UnicodeCombiningClassOverlay = UnicodeCombiningClassT 1
pattern UnicodeCombiningClassNukta = UnicodeCombiningClassT 7
pattern UnicodeCombiningClassKanaVoicing = UnicodeCombiningClassT 8
pattern UnicodeCombiningClassVirama = UnicodeCombiningClassT 9
pattern UnicodeCombiningClassCcc10 = UnicodeCombiningClassT 10
pattern UnicodeCombiningClassCcc11 = UnicodeCombiningClassT 11
pattern UnicodeCombiningClassCcc12 = UnicodeCombiningClassT 12
pattern UnicodeCombiningClassCcc13 = UnicodeCombiningClassT 13
pattern UnicodeCombiningClassCcc14 = UnicodeCombiningClassT 14
pattern UnicodeCombiningClassCcc15 = UnicodeCombiningClassT 15
pattern UnicodeCombiningClassCcc16 = UnicodeCombiningClassT 16
pattern UnicodeCombiningClassCcc17 = UnicodeCombiningClassT 17
pattern UnicodeCombiningClassCcc18 = UnicodeCombiningClassT 18
pattern UnicodeCombiningClassCcc19 = UnicodeCombiningClassT 19
pattern UnicodeCombiningClassCcc20 = UnicodeCombiningClassT 20
pattern UnicodeCombiningClassCcc21 = UnicodeCombiningClassT 21
pattern UnicodeCombiningClassCcc22 = UnicodeCombiningClassT 22
pattern UnicodeCombiningClassCcc23 = UnicodeCombiningClassT 23
pattern UnicodeCombiningClassCcc24 = UnicodeCombiningClassT 24
pattern UnicodeCombiningClassCcc25 = UnicodeCombiningClassT 25
pattern UnicodeCombiningClassCcc26 = UnicodeCombiningClassT 26
pattern UnicodeCombiningClassCcc27 = UnicodeCombiningClassT 27
pattern UnicodeCombiningClassCcc28 = UnicodeCombiningClassT 28
pattern UnicodeCombiningClassCcc29 = UnicodeCombiningClassT 29
pattern UnicodeCombiningClassCcc30 = UnicodeCombiningClassT 30
pattern UnicodeCombiningClassCcc31 = UnicodeCombiningClassT 31
pattern UnicodeCombiningClassCcc32 = UnicodeCombiningClassT 32
pattern UnicodeCombiningClassCcc33 = UnicodeCombiningClassT 33
pattern UnicodeCombiningClassCcc34 = UnicodeCombiningClassT 34
pattern UnicodeCombiningClassCcc35 = UnicodeCombiningClassT 35
pattern UnicodeCombiningClassCcc36 = UnicodeCombiningClassT 36
pattern UnicodeCombiningClassCcc84 = UnicodeCombiningClassT 84
pattern UnicodeCombiningClassCcc91 = UnicodeCombiningClassT 91
pattern UnicodeCombiningClassCcc103 = UnicodeCombiningClassT 103
pattern UnicodeCombiningClassCcc107 = UnicodeCombiningClassT 107
pattern UnicodeCombiningClassCcc118 = UnicodeCombiningClassT 118
pattern UnicodeCombiningClassCcc122 = UnicodeCombiningClassT 122
pattern UnicodeCombiningClassCcc129 = UnicodeCombiningClassT 129
pattern UnicodeCombiningClassCcc130 = UnicodeCombiningClassT 130
pattern UnicodeCombiningClassCcc133 = UnicodeCombiningClassT 132
pattern UnicodeCombiningClassAttachedBelowLeft = UnicodeCombiningClassT 200
pattern UnicodeCombiningClassAttachedBelow = UnicodeCombiningClassT 202
pattern UnicodeCombiningClassAttachedAbove = UnicodeCombiningClassT 214
pattern UnicodeCombiningClassAttachedAboveRight = UnicodeCombiningClassT 216
pattern UnicodeCombiningClassBelowLeft = UnicodeCombiningClassT 218
pattern UnicodeCombiningClassBelow = UnicodeCombiningClassT 220
pattern UnicodeCombiningClassBelowRight = UnicodeCombiningClassT 222
pattern UnicodeCombiningClassLeft = UnicodeCombiningClassT 224
pattern UnicodeCombiningClassRight = UnicodeCombiningClassT 226
pattern UnicodeCombiningClassAboveLeft = UnicodeCombiningClassT 228
pattern UnicodeCombiningClassAbove = UnicodeCombiningClassT 230
pattern UnicodeCombiningClassAboveRight = UnicodeCombiningClassT 232
pattern UnicodeCombiningClassDoubleBelow = UnicodeCombiningClassT 233
pattern UnicodeCombiningClassDoubleAbove = UnicodeCombiningClassT 234
pattern UnicodeCombiningClassIotaSubscript = UnicodeCombiningClassT 240
pattern UnicodeCombiningClassInvalid = UnicodeCombiningClassT 255

data UnicodeFuncs

newtype UnicodeGeneralCategory = UnicodeGeneralCategoryT{ unwrapUnicodeGeneralCategory :: CInt }
pattern UnicodeGeneralCategoryControl = UnicodeGeneralCategoryT 0
pattern UnicodeGeneralCategoryFormat = UnicodeGeneralCategoryT 1
pattern UnicodeGeneralCategoryUnassigned = UnicodeGeneralCategoryT 2
pattern UnicodeGeneralCategoryPrivateUse = UnicodeGeneralCategoryT 3
pattern UnicodeGeneralCategorySurrogate = UnicodeGeneralCategoryT 4
pattern UnicodeGeneralCategoryLowercaseLetter = UnicodeGeneralCategoryT 5
pattern UnicodeGeneralCategoryModifierLetter = UnicodeGeneralCategoryT 6
pattern UnicodeGeneralCategoryOtherLetter = UnicodeGeneralCategoryT 7
pattern UnicodeGeneralCategoryTitlecaseLetter = UnicodeGeneralCategoryT 8
pattern UnicodeGeneralCategoryUppercaseLetter = UnicodeGeneralCategoryT 9
pattern UnicodeGeneralCategorySpacingMark = UnicodeGeneralCategoryT 10
pattern UnicodeGeneralCategoryEnclosingMark = UnicodeGeneralCategoryT 11
pattern UnicodeGeneralCategoryNonSpacingMark = UnicodeGeneralCategoryT 12
pattern UnicodeGeneralCategoryDecimalNumber = UnicodeGeneralCategoryT 13
pattern UnicodeGeneralCategoryLetterNumber = UnicodeGeneralCategoryT 14
pattern UnicodeGeneralCategoryOtherNumber = UnicodeGeneralCategoryT 15
pattern UnicodeGeneralCategoryConnectPunctuation = UnicodeGeneralCategoryT 16
pattern UnicodeGeneralCategoryDashPunctuation = UnicodeGeneralCategoryT 17
pattern UnicodeGeneralCategoryClosePunctuation = UnicodeGeneralCategoryT 18
pattern UnicodeGeneralCategoryFinalPunctuation = UnicodeGeneralCategoryT 19
pattern UnicodeGeneralCategoryInitialPunctuation = UnicodeGeneralCategoryT 20
pattern UnicodeGeneralCategoryOtherPunctuation = UnicodeGeneralCategoryT 21
pattern UnicodeGeneralCategoryOpenPunctuation = UnicodeGeneralCategoryT 22
pattern UnicodeGeneralCategoryCurrencySymbol = UnicodeGeneralCategoryT 23
pattern UnicodeGeneralCategoryModifierSymbol = UnicodeGeneralCategoryT 24
pattern UnicodeGeneralCategoryMathSymbol = UnicodeGeneralCategoryT 25
pattern UnicodeGeneralCategoryOtherSymbol = UnicodeGeneralCategoryT 26
pattern UnicodeGeneralCategoryLineSeparator = UnicodeGeneralCategoryT 27
pattern UnicodeGeneralCategoryParagraphSeparator = UnicodeGeneralCategoryT 28
pattern UnicodeGeneralCategorySpaceSeparator = UnicodeGeneralCategoryT 29

data UserDataKey

data Variation

foreign import ccall "hb_tag_from_string" tagFromString :: CString -> CInt -> IO CUInt
foreign import ccall "hb_tag_to_string" tagToString :: CUInt -> CString -> IO ()
foreign import ccall "hb_direction_from_string" directionFromString :: CString -> CInt -> IO Direction
foreign import ccall "hb_direction_to_string" directionToString :: Direction -> IO CString
foreign import ccall "hb_language_from_string" languageFromString :: CString -> CInt -> IO (Ptr LanguageImpl)
foreign import ccall "hb_language_to_string" languageToString :: Ptr LanguageImpl -> IO CString
foreign import ccall "hb_language_get_default" languageGetDefault :: IO (Ptr LanguageImpl)
foreign import ccall "hb_script_from_iso15924_tag" scriptFromIso15924Tag :: CUInt -> IO Script
foreign import ccall "hb_script_from_string" scriptFromString :: CString -> CInt -> IO Script
foreign import ccall "hb_script_to_iso15924_tag" scriptToIso15924Tag :: Script -> IO CUInt
foreign import ccall "hb_script_get_horizontal_direction" scriptGetHorizontalDirection :: Script -> IO Direction
foreign import ccall "hb_feature_from_string" featureFromString :: CString -> CInt -> Ptr Feature -> IO CInt
foreign import ccall "hb_feature_to_string" featureToString :: Ptr Feature -> CString -> CUInt -> IO ()
foreign import ccall "hb_variation_from_string" variationFromString :: CString -> CInt -> Ptr Variation -> IO CInt
foreign import ccall "hb_variation_to_string" variationToString :: Ptr Variation -> CString -> CUInt -> IO ()
foreign import ccall "hb_color_get_alpha" colorGetAlpha :: CUInt -> IO CUChar
foreign import ccall "hb_color_get_red" colorGetRed :: CUInt -> IO CUChar
foreign import ccall "hb_color_get_green" colorGetGreen :: CUInt -> IO CUChar
foreign import ccall "hb_color_get_blue" colorGetBlue :: CUInt -> IO CUChar
foreign import ccall "hb_blob_create" blobCreate :: CString -> CUInt -> MemoryMode -> Ptr a -> FunPtr (Ptr b -> IO ()) -> IO (Ptr Blob)
foreign import ccall "hb_blob_create_from_file" blobCreateFromFile :: CString -> IO (Ptr Blob)
foreign import ccall "hb_blob_create_sub_blob" blobCreateSubBlob :: Ptr Blob -> CUInt -> CUInt -> IO (Ptr Blob)
foreign import ccall "hb_blob_copy_writable_or_fail" blobCopyWritableOrFail :: Ptr Blob -> IO (Ptr Blob)
foreign import ccall "hb_blob_get_empty" blobGetEmpty :: IO (Ptr Blob)
foreign import ccall "hb_blob_reference" blobReference :: Ptr Blob -> IO (Ptr Blob)
foreign import ccall "hb_blob_destroy" blobDestroy :: Ptr Blob -> IO ()
foreign import ccall "hb_blob_set_user_data" blobSetUserData :: Ptr Blob -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_blob_get_user_data" blobGetUserData :: Ptr Blob -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_blob_make_immutable" blobMakeImmutable :: Ptr Blob -> IO ()
foreign import ccall "hb_blob_is_immutable" blobIsImmutable :: Ptr Blob -> IO CInt
foreign import ccall "hb_blob_get_length" blobGetLength :: Ptr Blob -> IO CUInt
foreign import ccall "hb_blob_get_data" blobGetData :: Ptr Blob -> Ptr CUInt -> IO CString
foreign import ccall "hb_blob_get_data_writable" blobGetDataWritable :: Ptr Blob -> Ptr CUInt -> IO CString
foreign import ccall "hb_unicode_funcs_get_default" unicodeFuncsGetDefault :: IO (Ptr UnicodeFuncs)
foreign import ccall "hb_unicode_funcs_create" unicodeFuncsCreate :: Ptr UnicodeFuncs -> IO (Ptr UnicodeFuncs)
foreign import ccall "hb_unicode_funcs_get_empty" unicodeFuncsGetEmpty :: IO (Ptr UnicodeFuncs)
foreign import ccall "hb_unicode_funcs_reference" unicodeFuncsReference :: Ptr UnicodeFuncs -> IO (Ptr UnicodeFuncs)
foreign import ccall "hb_unicode_funcs_destroy" unicodeFuncsDestroy :: Ptr UnicodeFuncs -> IO ()
foreign import ccall "hb_unicode_funcs_set_user_data" unicodeFuncsSetUserData :: Ptr UnicodeFuncs -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_unicode_funcs_get_user_data" unicodeFuncsGetUserData :: Ptr UnicodeFuncs -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_unicode_funcs_make_immutable" unicodeFuncsMakeImmutable :: Ptr UnicodeFuncs -> IO ()
foreign import ccall "hb_unicode_funcs_is_immutable" unicodeFuncsIsImmutable :: Ptr UnicodeFuncs -> IO CInt
foreign import ccall "hb_unicode_funcs_get_parent" unicodeFuncsGetParent :: Ptr UnicodeFuncs -> IO (Ptr UnicodeFuncs)
foreign import ccall "hb_unicode_funcs_set_combining_class_func" unicodeFuncsSetCombiningClassFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr a -> IO UnicodeCombiningClass) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_funcs_set_general_category_func" unicodeFuncsSetGeneralCategoryFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr a -> IO UnicodeGeneralCategory) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_funcs_set_mirroring_func" unicodeFuncsSetMirroringFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr a -> IO CUInt) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_funcs_set_script_func" unicodeFuncsSetScriptFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr a -> IO Script) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_funcs_set_compose_func" unicodeFuncsSetComposeFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> CUInt -> Ptr CUInt -> Ptr a -> IO CInt) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_funcs_set_decompose_func" unicodeFuncsSetDecomposeFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr CUInt -> Ptr CUInt -> Ptr a -> IO CInt) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_combining_class" unicodeCombiningClass :: Ptr UnicodeFuncs -> CUInt -> IO UnicodeCombiningClass
foreign import ccall "hb_unicode_general_category" unicodeGeneralCategory :: Ptr UnicodeFuncs -> CUInt -> IO UnicodeGeneralCategory
foreign import ccall "hb_unicode_mirroring" unicodeMirroring :: Ptr UnicodeFuncs -> CUInt -> IO CUInt
foreign import ccall "hb_unicode_script" unicodeScript :: Ptr UnicodeFuncs -> CUInt -> IO Script
foreign import ccall "hb_unicode_compose" unicodeCompose :: Ptr UnicodeFuncs -> CUInt -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_unicode_decompose" unicodeDecompose :: Ptr UnicodeFuncs -> CUInt -> Ptr CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_set_create" setCreate :: IO (Ptr Set)
foreign import ccall "hb_set_get_empty" setGetEmpty :: IO (Ptr Set)
foreign import ccall "hb_set_reference" setReference :: Ptr Set -> IO (Ptr Set)
foreign import ccall "hb_set_destroy" setDestroy :: Ptr Set -> IO ()
foreign import ccall "hb_set_set_user_data" setSetUserData :: Ptr Set -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_set_get_user_data" setGetUserData :: Ptr Set -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_set_allocation_successful" setAllocationSuccessful :: Ptr Set -> IO CInt
foreign import ccall "hb_set_clear" setClear :: Ptr Set -> IO ()
foreign import ccall "hb_set_is_empty" setIsEmpty :: Ptr Set -> IO CInt
foreign import ccall "hb_set_has" setHas :: Ptr Set -> CUInt -> IO CInt
foreign import ccall "hb_set_add" setAdd :: Ptr Set -> CUInt -> IO ()
foreign import ccall "hb_set_add_range" setAddRange :: Ptr Set -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_set_del" setDel :: Ptr Set -> CUInt -> IO ()
foreign import ccall "hb_set_del_range" setDelRange :: Ptr Set -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_set_is_equal" setIsEqual :: Ptr Set -> Ptr Set -> IO CInt
foreign import ccall "hb_set_is_subset" setIsSubset :: Ptr Set -> Ptr Set -> IO CInt
foreign import ccall "hb_set_set" setSet :: Ptr Set -> Ptr Set -> IO ()
foreign import ccall "hb_set_union" setUnion :: Ptr Set -> Ptr Set -> IO ()
foreign import ccall "hb_set_intersect" setIntersect :: Ptr Set -> Ptr Set -> IO ()
foreign import ccall "hb_set_subtract" setSubtract :: Ptr Set -> Ptr Set -> IO ()
foreign import ccall "hb_set_symmetric_difference" setSymmetricDifference :: Ptr Set -> Ptr Set -> IO ()
foreign import ccall "hb_set_get_population" setGetPopulation :: Ptr Set -> IO CUInt
foreign import ccall "hb_set_get_min" setGetMin :: Ptr Set -> IO CUInt
foreign import ccall "hb_set_get_max" setGetMax :: Ptr Set -> IO CUInt
foreign import ccall "hb_set_next" setNext :: Ptr Set -> Ptr CUInt -> IO CInt
foreign import ccall "hb_set_previous" setPrevious :: Ptr Set -> Ptr CUInt -> IO CInt
foreign import ccall "hb_set_next_range" setNextRange :: Ptr Set -> Ptr CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_set_previous_range" setPreviousRange :: Ptr Set -> Ptr CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_face_count" faceCount :: Ptr Blob -> IO CUInt
foreign import ccall "hb_face_create" faceCreate :: Ptr Blob -> CUInt -> IO (Ptr Face)
foreign import ccall "hb_face_create_for_tables" faceCreateForTables :: FunPtr (Ptr Face -> CUInt -> Ptr a -> IO (Ptr Blob)) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO (Ptr Face)
foreign import ccall "hb_face_get_empty" faceGetEmpty :: IO (Ptr Face)
foreign import ccall "hb_face_reference" faceReference :: Ptr Face -> IO (Ptr Face)
foreign import ccall "hb_face_destroy" faceDestroy :: Ptr Face -> IO ()
foreign import ccall "hb_face_set_user_data" faceSetUserData :: Ptr Face -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_face_get_user_data" faceGetUserData :: Ptr Face -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_face_make_immutable" faceMakeImmutable :: Ptr Face -> IO ()
foreign import ccall "hb_face_is_immutable" faceIsImmutable :: Ptr Face -> IO CInt
foreign import ccall "hb_face_reference_table" faceReferenceTable :: Ptr Face -> CUInt -> IO (Ptr Blob)
foreign import ccall "hb_face_reference_blob" faceReferenceBlob :: Ptr Face -> IO (Ptr Blob)
foreign import ccall "hb_face_set_index" faceSetIndex :: Ptr Face -> CUInt -> IO ()
foreign import ccall "hb_face_get_index" faceGetIndex :: Ptr Face -> IO CUInt
foreign import ccall "hb_face_set_upem" faceSetUpem :: Ptr Face -> CUInt -> IO ()
foreign import ccall "hb_face_get_upem" faceGetUpem :: Ptr Face -> IO CUInt
foreign import ccall "hb_face_set_glyph_count" faceSetGlyphCount :: Ptr Face -> CUInt -> IO ()
foreign import ccall "hb_face_get_glyph_count" faceGetGlyphCount :: Ptr Face -> IO CUInt
foreign import ccall "hb_face_get_table_tags" faceGetTableTags :: Ptr Face -> CUInt -> Ptr CUInt -> Ptr CUInt -> IO CUInt
foreign import ccall "hb_face_collect_unicodes" faceCollectUnicodes :: Ptr Face -> Ptr Set -> IO ()
foreign import ccall "hb_face_collect_variation_selectors" faceCollectVariationSelectors :: Ptr Face -> Ptr Set -> IO ()
foreign import ccall "hb_face_collect_variation_unicodes" faceCollectVariationUnicodes :: Ptr Face -> CUInt -> Ptr Set -> IO ()
foreign import ccall "hb_face_builder_create" faceBuilderCreate :: IO (Ptr Face)
foreign import ccall "hb_face_builder_add_table" faceBuilderAddTable :: Ptr Face -> CUInt -> Ptr Blob -> IO CInt
foreign import ccall "hb_font_funcs_create" fontFuncsCreate :: IO (Ptr FontFuncs)
foreign import ccall "hb_font_funcs_get_empty" fontFuncsGetEmpty :: IO (Ptr FontFuncs)
foreign import ccall "hb_font_funcs_reference" fontFuncsReference :: Ptr FontFuncs -> IO (Ptr FontFuncs)
foreign import ccall "hb_font_funcs_destroy" fontFuncsDestroy :: Ptr FontFuncs -> IO ()
foreign import ccall "hb_font_funcs_set_user_data" fontFuncsSetUserData :: Ptr FontFuncs -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_font_funcs_get_user_data" fontFuncsGetUserData :: Ptr FontFuncs -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_font_funcs_make_immutable" fontFuncsMakeImmutable :: Ptr FontFuncs -> IO ()
foreign import ccall "hb_font_funcs_is_immutable" fontFuncsIsImmutable :: Ptr FontFuncs -> IO CInt
foreign import ccall "hb_font_funcs_set_font_h_extents_func" fontFuncsSetFontHExtentsFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> Ptr FontExtents -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_font_v_extents_func" fontFuncsSetFontVExtentsFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> Ptr FontExtents -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_nominal_glyph_func" fontFuncsSetNominalGlyphFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_nominal_glyphs_func" fontFuncsSetNominalGlyphsFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr CUInt -> CUInt -> Ptr CUInt -> CUInt -> Ptr b -> IO CUInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_variation_glyph_func" fontFuncsSetVariationGlyphFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> CUInt -> Ptr CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_h_advance_func" fontFuncsSetGlyphHAdvanceFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_v_advance_func" fontFuncsSetGlyphVAdvanceFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_h_advances_func" fontFuncsSetGlyphHAdvancesFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr CUInt -> CUInt -> Ptr CInt -> CUInt -> Ptr b -> IO ()) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_v_advances_func" fontFuncsSetGlyphVAdvancesFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr CUInt -> CUInt -> Ptr CInt -> CUInt -> Ptr b -> IO ()) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_h_origin_func" fontFuncsSetGlyphHOriginFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr CInt -> Ptr CInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_v_origin_func" fontFuncsSetGlyphVOriginFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr CInt -> Ptr CInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_h_kerning_func" fontFuncsSetGlyphHKerningFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_extents_func" fontFuncsSetGlyphExtentsFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> Ptr GlyphExtents -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_contour_point_func" fontFuncsSetGlyphContourPointFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> CUInt -> Ptr CInt -> Ptr CInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_name_func" fontFuncsSetGlyphNameFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> CString -> CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_from_name_func" fontFuncsSetGlyphFromNameFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CString -> CInt -> Ptr CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_get_h_extents" fontGetHExtents :: Ptr Font -> Ptr FontExtents -> IO CInt
foreign import ccall "hb_font_get_v_extents" fontGetVExtents :: Ptr Font -> Ptr FontExtents -> IO CInt
foreign import ccall "hb_font_get_nominal_glyph" fontGetNominalGlyph :: Ptr Font -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_font_get_variation_glyph" fontGetVariationGlyph :: Ptr Font -> CUInt -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_font_get_nominal_glyphs" fontGetNominalGlyphs :: Ptr Font -> CUInt -> Ptr CUInt -> CUInt -> Ptr CUInt -> CUInt -> IO CUInt
foreign import ccall "hb_font_get_glyph_h_advance" fontGetGlyphHAdvance :: Ptr Font -> CUInt -> IO CInt
foreign import ccall "hb_font_get_glyph_v_advance" fontGetGlyphVAdvance :: Ptr Font -> CUInt -> IO CInt
foreign import ccall "hb_font_get_glyph_h_advances" fontGetGlyphHAdvances :: Ptr Font -> CUInt -> Ptr CUInt -> CUInt -> Ptr CInt -> CUInt -> IO ()
foreign import ccall "hb_font_get_glyph_v_advances" fontGetGlyphVAdvances :: Ptr Font -> CUInt -> Ptr CUInt -> CUInt -> Ptr CInt -> CUInt -> IO ()
foreign import ccall "hb_font_get_glyph_h_origin" fontGetGlyphHOrigin :: Ptr Font -> CUInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "hb_font_get_glyph_v_origin" fontGetGlyphVOrigin :: Ptr Font -> CUInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "hb_font_get_glyph_h_kerning" fontGetGlyphHKerning :: Ptr Font -> CUInt -> CUInt -> IO CInt
foreign import ccall "hb_font_get_glyph_extents" fontGetGlyphExtents :: Ptr Font -> CUInt -> Ptr GlyphExtents -> IO CInt
foreign import ccall "hb_font_get_glyph_contour_point" fontGetGlyphContourPoint :: Ptr Font -> CUInt -> CUInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "hb_font_get_glyph_name" fontGetGlyphName :: Ptr Font -> CUInt -> CString -> CUInt -> IO CInt
foreign import ccall "hb_font_get_glyph_from_name" fontGetGlyphFromName :: Ptr Font -> CString -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_font_get_glyph" fontGetGlyph :: Ptr Font -> CUInt -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_font_get_extents_for_direction" fontGetExtentsForDirection :: Ptr Font -> Direction -> Ptr FontExtents -> IO ()
foreign import ccall "hb_font_get_glyph_advance_for_direction" fontGetGlyphAdvanceForDirection :: Ptr Font -> CUInt -> Direction -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "hb_font_get_glyph_advances_for_direction" fontGetGlyphAdvancesForDirection :: Ptr Font -> Direction -> CUInt -> Ptr CUInt -> CUInt -> Ptr CInt -> CUInt -> IO ()
foreign import ccall "hb_font_get_glyph_origin_for_direction" fontGetGlyphOriginForDirection :: Ptr Font -> CUInt -> Direction -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "hb_font_add_glyph_origin_for_direction" fontAddGlyphOriginForDirection :: Ptr Font -> CUInt -> Direction -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "hb_font_subtract_glyph_origin_for_direction" fontSubtractGlyphOriginForDirection :: Ptr Font -> CUInt -> Direction -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "hb_font_get_glyph_kerning_for_direction" fontGetGlyphKerningForDirection :: Ptr Font -> CUInt -> CUInt -> Direction -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "hb_font_get_glyph_extents_for_origin" fontGetGlyphExtentsForOrigin :: Ptr Font -> CUInt -> Direction -> Ptr GlyphExtents -> IO CInt
foreign import ccall "hb_font_get_glyph_contour_point_for_origin" fontGetGlyphContourPointForOrigin :: Ptr Font -> CUInt -> CUInt -> Direction -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall "hb_font_glyph_to_string" fontGlyphToString :: Ptr Font -> CUInt -> CString -> CUInt -> IO ()
foreign import ccall "hb_font_glyph_from_string" fontGlyphFromString :: Ptr Font -> CString -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "hb_font_create" fontCreate :: Ptr Face -> IO (Ptr Font)
foreign import ccall "hb_font_create_sub_font" fontCreateSubFont :: Ptr Font -> IO (Ptr Font)
foreign import ccall "hb_font_get_empty" fontGetEmpty :: IO (Ptr Font)
foreign import ccall "hb_font_reference" fontReference :: Ptr Font -> IO (Ptr Font)
foreign import ccall "hb_font_destroy" fontDestroy :: Ptr Font -> IO ()
foreign import ccall "hb_font_set_user_data" fontSetUserData :: Ptr Font -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_font_get_user_data" fontGetUserData :: Ptr Font -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_font_make_immutable" fontMakeImmutable :: Ptr Font -> IO ()
foreign import ccall "hb_font_is_immutable" fontIsImmutable :: Ptr Font -> IO CInt
foreign import ccall "hb_font_set_parent" fontSetParent :: Ptr Font -> Ptr Font -> IO ()
foreign import ccall "hb_font_get_parent" fontGetParent :: Ptr Font -> IO (Ptr Font)
foreign import ccall "hb_font_set_face" fontSetFace :: Ptr Font -> Ptr Face -> IO ()
foreign import ccall "hb_font_get_face" fontGetFace :: Ptr Font -> IO (Ptr Face)
foreign import ccall "hb_font_set_funcs" fontSetFuncs :: Ptr Font -> Ptr FontFuncs -> Ptr a -> FunPtr (Ptr b -> IO ()) -> IO ()
foreign import ccall "hb_font_set_funcs_data" fontSetFuncsData :: Ptr Font -> Ptr a -> FunPtr (Ptr b -> IO ()) -> IO ()
foreign import ccall "hb_font_set_scale" fontSetScale :: Ptr Font -> CInt -> CInt -> IO ()
foreign import ccall "hb_font_get_scale" fontGetScale :: Ptr Font -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "hb_font_set_ppem" fontSetPpem :: Ptr Font -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_font_get_ppem" fontGetPpem :: Ptr Font -> Ptr CUInt -> Ptr CUInt -> IO ()
foreign import ccall "hb_font_set_ptem" fontSetPtem :: Ptr Font -> CFloat -> IO ()
foreign import ccall "hb_font_get_ptem" fontGetPtem :: Ptr Font -> IO CFloat
foreign import ccall "hb_font_set_variations" fontSetVariations :: Ptr Font -> Ptr Variation -> CUInt -> IO ()
foreign import ccall "hb_font_set_var_coords_design" fontSetVarCoordsDesign :: Ptr Font -> Ptr CFloat -> CUInt -> IO ()
foreign import ccall "hb_font_set_var_coords_normalized" fontSetVarCoordsNormalized :: Ptr Font -> Ptr CInt -> CUInt -> IO ()
foreign import ccall "hb_font_get_var_coords_normalized" fontGetVarCoordsNormalized :: Ptr Font -> Ptr CUInt -> IO (Ptr CInt)
foreign import ccall "hb_font_set_var_named_instance" fontSetVarNamedInstance :: Ptr Font -> CUInt -> IO ()
foreign import ccall "hb_glyph_info_get_glyph_flags" glyphInfoGetGlyphFlags :: Ptr GlyphInfo -> IO GlyphFlags
foreign import ccall "hb_segment_properties_equal" segmentPropertiesEqual :: Ptr SegmentProperties -> Ptr SegmentProperties -> IO CInt
foreign import ccall "hb_segment_properties_hash" segmentPropertiesHash :: Ptr SegmentProperties -> IO CUInt
foreign import ccall "hb_buffer_create" bufferCreate :: IO (Ptr Buffer)
foreign import ccall "hb_buffer_get_empty" bufferGetEmpty :: IO (Ptr Buffer)
foreign import ccall "hb_buffer_reference" bufferReference :: Ptr Buffer -> IO (Ptr Buffer)
foreign import ccall "hb_buffer_destroy" bufferDestroy :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_set_user_data" bufferSetUserData :: Ptr Buffer -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_buffer_get_user_data" bufferGetUserData :: Ptr Buffer -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_buffer_set_content_type" bufferSetContentType :: Ptr Buffer -> BufferContentType -> IO ()
foreign import ccall "hb_buffer_get_content_type" bufferGetContentType :: Ptr Buffer -> IO BufferContentType
foreign import ccall "hb_buffer_set_unicode_funcs" bufferSetUnicodeFuncs :: Ptr Buffer -> Ptr UnicodeFuncs -> IO ()
foreign import ccall "hb_buffer_get_unicode_funcs" bufferGetUnicodeFuncs :: Ptr Buffer -> IO (Ptr UnicodeFuncs)
foreign import ccall "hb_buffer_set_direction" bufferSetDirection :: Ptr Buffer -> Direction -> IO ()
foreign import ccall "hb_buffer_get_direction" bufferGetDirection :: Ptr Buffer -> IO Direction
foreign import ccall "hb_buffer_set_script" bufferSetScript :: Ptr Buffer -> Script -> IO ()
foreign import ccall "hb_buffer_get_script" bufferGetScript :: Ptr Buffer -> IO Script
foreign import ccall "hb_buffer_set_language" bufferSetLanguage :: Ptr Buffer -> Ptr LanguageImpl -> IO ()
foreign import ccall "hb_buffer_get_language" bufferGetLanguage :: Ptr Buffer -> IO (Ptr LanguageImpl)
foreign import ccall "hb_buffer_set_segment_properties" bufferSetSegmentProperties :: Ptr Buffer -> Ptr SegmentProperties -> IO ()
foreign import ccall "hb_buffer_get_segment_properties" bufferGetSegmentProperties :: Ptr Buffer -> Ptr SegmentProperties -> IO ()
foreign import ccall "hb_buffer_guess_segment_properties" bufferGuessSegmentProperties :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_set_flags" bufferSetFlags :: Ptr Buffer -> BufferFlags -> IO ()
foreign import ccall "hb_buffer_get_flags" bufferGetFlags :: Ptr Buffer -> IO BufferFlags
foreign import ccall "hb_buffer_set_cluster_level" bufferSetClusterLevel :: Ptr Buffer -> BufferClusterLevel -> IO ()
foreign import ccall "hb_buffer_get_cluster_level" bufferGetClusterLevel :: Ptr Buffer -> IO BufferClusterLevel
foreign import ccall "hb_buffer_set_replacement_codepoint" bufferSetReplacementCodepoint :: Ptr Buffer -> CUInt -> IO ()
foreign import ccall "hb_buffer_get_replacement_codepoint" bufferGetReplacementCodepoint :: Ptr Buffer -> IO CUInt
foreign import ccall "hb_buffer_set_invisible_glyph" bufferSetInvisibleGlyph :: Ptr Buffer -> CUInt -> IO ()
foreign import ccall "hb_buffer_get_invisible_glyph" bufferGetInvisibleGlyph :: Ptr Buffer -> IO CUInt
foreign import ccall "hb_buffer_reset" bufferReset :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_clear_contents" bufferClearContents :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_pre_allocate" bufferPreAllocate :: Ptr Buffer -> CUInt -> IO CInt
foreign import ccall "hb_buffer_allocation_successful" bufferAllocationSuccessful :: Ptr Buffer -> IO CInt
foreign import ccall "hb_buffer_reverse" bufferReverse :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_reverse_range" bufferReverseRange :: Ptr Buffer -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_buffer_reverse_clusters" bufferReverseClusters :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_add" bufferAdd :: Ptr Buffer -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_buffer_add_utf8" bufferAddUtf8 :: Ptr Buffer -> CString -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall "hb_buffer_add_utf16" bufferAddUtf16 :: Ptr Buffer -> Ptr CUShort -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall "hb_buffer_add_utf32" bufferAddUtf32 :: Ptr Buffer -> Ptr CUInt -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall "hb_buffer_add_latin1" bufferAddLatin1 :: Ptr Buffer -> Ptr CUChar -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall "hb_buffer_add_codepoints" bufferAddCodepoints :: Ptr Buffer -> Ptr CUInt -> CInt -> CUInt -> CInt -> IO ()
foreign import ccall "hb_buffer_append" bufferAppend :: Ptr Buffer -> Ptr Buffer -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_buffer_set_length" bufferSetLength :: Ptr Buffer -> CUInt -> IO CInt
foreign import ccall "hb_buffer_get_length" bufferGetLength :: Ptr Buffer -> IO CUInt
foreign import ccall "hb_buffer_get_glyph_infos" bufferGetGlyphInfos :: Ptr Buffer -> Ptr CUInt -> IO (Ptr GlyphInfo)
foreign import ccall "hb_buffer_get_glyph_positions" bufferGetGlyphPositions :: Ptr Buffer -> Ptr CUInt -> IO (Ptr GlyphPosition)
foreign import ccall "hb_buffer_normalize_glyphs" bufferNormalizeGlyphs :: Ptr Buffer -> IO ()
foreign import ccall "hb_buffer_serialize_format_from_string" bufferSerializeFormatFromString :: CString -> CInt -> IO BufferSerializeFormat
foreign import ccall "hb_buffer_serialize_format_to_string" bufferSerializeFormatToString :: BufferSerializeFormat -> IO CString
foreign import ccall "hb_buffer_serialize_list_formats" bufferSerializeListFormats :: IO (Ptr CString)
foreign import ccall "hb_buffer_serialize_glyphs" bufferSerializeGlyphs :: Ptr Buffer -> CUInt -> CUInt -> CString -> CUInt -> Ptr CUInt -> Ptr Font -> BufferSerializeFormat -> BufferSerializeFlags -> IO CUInt
foreign import ccall "hb_buffer_deserialize_glyphs" bufferDeserializeGlyphs :: Ptr Buffer -> CString -> CInt -> Ptr CString -> Ptr Font -> BufferSerializeFormat -> IO CInt
foreign import ccall "hb_buffer_diff" bufferDiff :: Ptr Buffer -> Ptr Buffer -> CUInt -> CUInt -> IO BufferDiffFlags
foreign import ccall "hb_buffer_set_message_func" bufferSetMessageFunc :: Ptr Buffer -> FunPtr (Ptr Buffer -> Ptr Font -> CString -> Ptr a -> IO CInt) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_font_funcs_set_glyph_func" fontFuncsSetGlyphFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> CUInt -> Ptr CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_set_invert" setInvert :: Ptr Set -> IO ()
foreign import ccall "hb_unicode_funcs_set_eastasian_width_func" unicodeFuncsSetEastasianWidthFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr a -> IO CUInt) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_eastasian_width" unicodeEastasianWidth :: Ptr UnicodeFuncs -> CUInt -> IO CUInt
foreign import ccall "hb_unicode_funcs_set_decompose_compatibility_func" unicodeFuncsSetDecomposeCompatibilityFunc :: Ptr UnicodeFuncs -> FunPtr (Ptr UnicodeFuncs -> CUInt -> Ptr CUInt -> Ptr a -> IO CUInt) -> Ptr b -> FunPtr (Ptr c -> IO ()) -> IO ()
foreign import ccall "hb_unicode_decompose_compatibility" unicodeDecomposeCompatibility :: Ptr UnicodeFuncs -> CUInt -> Ptr CUInt -> IO CUInt
foreign import ccall "hb_font_funcs_set_glyph_v_kerning_func" fontFuncsSetGlyphVKerningFunc :: Ptr FontFuncs -> FunPtr (Ptr Font -> Ptr a -> CUInt -> CUInt -> Ptr b -> IO CInt) -> Ptr c -> FunPtr (Ptr d -> IO ()) -> IO ()
foreign import ccall "hb_font_get_glyph_v_kerning" fontGetGlyphVKerning :: Ptr Font -> CUInt -> CUInt -> IO CInt
foreign import ccall "hb_map_create" mapCreate :: IO (Ptr Map)
foreign import ccall "hb_map_get_empty" mapGetEmpty :: IO (Ptr Map)
foreign import ccall "hb_map_reference" mapReference :: Ptr Map -> IO (Ptr Map)
foreign import ccall "hb_map_destroy" mapDestroy :: Ptr Map -> IO ()
foreign import ccall "hb_map_set_user_data" mapSetUserData :: Ptr Map -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_map_get_user_data" mapGetUserData :: Ptr Map -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_map_allocation_successful" mapAllocationSuccessful :: Ptr Map -> IO CInt
foreign import ccall "hb_map_clear" mapClear :: Ptr Map -> IO ()
foreign import ccall "hb_map_is_empty" mapIsEmpty :: Ptr Map -> IO CInt
foreign import ccall "hb_map_get_population" mapGetPopulation :: Ptr Map -> IO CUInt
foreign import ccall "hb_map_set" mapSet :: Ptr Map -> CUInt -> CUInt -> IO ()
foreign import ccall "hb_map_get" mapGet :: Ptr Map -> CUInt -> IO CUInt
foreign import ccall "hb_map_del" mapDel :: Ptr Map -> CUInt -> IO ()
foreign import ccall "hb_map_has" mapHas :: Ptr Map -> CUInt -> IO CInt
foreign import ccall "hb_shape" shape :: Ptr Font -> Ptr Buffer -> Ptr Feature -> CUInt -> IO ()
foreign import ccall "hb_shape_full" shapeFull :: Ptr Font -> Ptr Buffer -> Ptr Feature -> CUInt -> Ptr CString -> IO CInt
foreign import ccall "hb_shape_list_shapers" shapeListShapers :: IO (Ptr CString)
foreign import ccall "hb_shape_plan_create" shapePlanCreate :: Ptr Face -> Ptr SegmentProperties -> Ptr Feature -> CUInt -> Ptr CString -> IO (Ptr ShapePlan)
foreign import ccall "hb_shape_plan_create_cached" shapePlanCreateCached :: Ptr Face -> Ptr SegmentProperties -> Ptr Feature -> CUInt -> Ptr CString -> IO (Ptr ShapePlan)
foreign import ccall "hb_shape_plan_create2" shapePlanCreate2 :: Ptr Face -> Ptr SegmentProperties -> Ptr Feature -> CUInt -> Ptr CInt -> CUInt -> Ptr CString -> IO (Ptr ShapePlan)
foreign import ccall "hb_shape_plan_create_cached2" shapePlanCreateCached2 :: Ptr Face -> Ptr SegmentProperties -> Ptr Feature -> CUInt -> Ptr CInt -> CUInt -> Ptr CString -> IO (Ptr ShapePlan)
foreign import ccall "hb_shape_plan_get_empty" shapePlanGetEmpty :: IO (Ptr ShapePlan)
foreign import ccall "hb_shape_plan_reference" shapePlanReference :: Ptr ShapePlan -> IO (Ptr ShapePlan)
foreign import ccall "hb_shape_plan_destroy" shapePlanDestroy :: Ptr ShapePlan -> IO ()
foreign import ccall "hb_shape_plan_set_user_data" shapePlanSetUserData :: Ptr ShapePlan -> Ptr UserDataKey -> Ptr a -> FunPtr (Ptr b -> IO ()) -> CInt -> IO CInt
foreign import ccall "hb_shape_plan_get_user_data" shapePlanGetUserData :: Ptr ShapePlan -> Ptr UserDataKey -> IO (Ptr a)
foreign import ccall "hb_shape_plan_execute" shapePlanExecute :: Ptr ShapePlan -> Ptr Font -> Ptr Buffer -> Ptr Feature -> CUInt -> IO CInt
foreign import ccall "hb_shape_plan_get_shaper" shapePlanGetShaper :: Ptr ShapePlan -> IO CString
foreign import ccall "hb_version" version :: Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()
foreign import ccall "hb_version_string" versionString :: IO CString
foreign import ccall "hb_version_atleast" versionAtleast :: CUInt -> CUInt -> CUInt -> IO CInt
