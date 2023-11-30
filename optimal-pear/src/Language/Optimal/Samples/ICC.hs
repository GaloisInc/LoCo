{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.ICC where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (first)
import Data.Bits (Bits (shiftL, shiftR, testBit, (.&.), (.|.)))
import Data.Int (Int32)
import Data.Ratio (Ratio, (%))
import Data.Word (Word16, Word32, Word64, Word8)
import Debug.Trace (traceShowId)
import GHC.Float (castWord32ToFloat)
import GHC.Real (infinity)
import Language.Optimal.Quote (optimal)
import Language.PEAR.API
  ( DRP,
    PT,
    SRP (..),
    VR (..),
    appDRP',
    appSRP,
    appSRP',
    applyToContents,
    except,
    mkPrimDRP,
    mkPrimSRP,
    mkPrimSRP',
    pairSRPs,
  )
import Language.PEAR.Region.API (Region, rDrop, rTake)
import Language.PEAR.Types (MaxWidth (..), WidthConstraint (..), dynWC)
import Language.PEAR.Util (Possibly)
import System.IO (IOMode (..), hGetContents, withBinaryFile)
import Thunk.RefVal (Thunked, delayAction, delayTuple, force)
import Thunk.Vector

driver :: (Region -> PT IO a) -> IO a
driver run =
  do
    withBinaryFile "sRGB_D65_colorimetric.icc" ReadMode $ \hdl ->
      do
        contents <- hGetContents hdl
        applyToContents run contents >>= \case
          Left err -> error (unlines err)
          Right res -> pure res

sample :: IO ()
sample = driver $ \r ->
  do
    icc <- mkOptimalICC r
    debug icc

--------------------------------------------------------------------------------

mkOptimalProfileHeader :: MonadIO m => Region -> PT m (OptimalProfileHeader (PT m))
mkOptimalICC :: MonadIO m => Region -> PT m (OptimalICC (PT m))
mkOptimalTagTable :: MonadIO m => Region -> PT m (OptimalTagTable (PT m))
mkOptimalTagHeader :: MonadIO m => Region -> PT m (OptimalTagHeader (PT m))
mkOptimalTag :: MonadIO m => Region -> Int -> PT m (OptimalTag (PT m))
mkOptimalTagBodyFromHeader :: MonadIO m => Region -> OptimalTagHeader (PT m) -> PT m (OptimalTagBody (PT m))

-- getTagHeader :: MonadIO m => OptimalICC m -> Int -> m (OptimalTagHeader m)
-- getTagBody :: MonadIO m => OptimalICC m -> Int -> m (OptimalTagBody m)

[optimal|
type OptimalICC = {
  profileHeader : OptimalProfileHeader,
  tagTable : OptimalTagTable,
}

mkOptimalICC : Region -> OptimalICC
mkOptimalICC region = {
  profileHeaderRegion = <| slice region Nothing (Just 128) |>,
  profileHeader       = module mkOptimalProfileHeader profileHeaderRegion,

  tagTableRegion = <| slice region (Just 128) Nothing |>,
  tagTable       = module mkOptimalTagTable tagTableRegion
}


type OptimalProfileHeader = {
  phSize : Word32,
  phPreferredCMMType : PreferredCMMType,
  phProfileVersion : Version,
  phProfileClass : ProfileClass,
  phColorSpace : ColorSpace,
  phPCS : PCS,
  phDateTime : DateTime,
  phPrimaryPlatform : PrimaryPlatform,
  phProfileFlags : ProfileFlags,
  phDeviceManufacturer : DeviceManufacturer,
  phDeviceModel : DeviceModel,
  phDeviceAttributes : DeviceAttributes,
  phRenderingIntent : RenderingIntent,
  phIlluminant : Illuminant,
  phCreator : Creator,
  phIdentifier : Identifier,
  phSpectralPCS : SpectralPCS,
  phSpectralPCSWavelength : SpectralPCSWavelength,
  phBiSpectralPCSWavelength : BiSpectralPCSWavelength,
  phMCSSignature : MCSSignature,
  phProfileSubclass : ProfileSubclass,
}

mkOptimalProfileHeader : Region -> OptimalProfileHeader
mkOptimalProfileHeader region = {
  phSizeRegion = <| slice region Nothing (Just 4) |>,
  phSize       = <| parseWord32 @$$ phSizeRegion |>,
  
  phPreferredCMMTypeRegion = <| slice region (Just 4) (Just 8) |>,
  phPreferredCMMType       = <| parsePreferredCMMType @$$ phPreferredCMMTypeRegion |>,

  phProfileVersionRegion = <| slice region (Just 8) (Just 12) |>,
  phProfileVersion       = <| parseVersion @$$ phProfileVersionRegion |>,
  
  phProfileClassRegion = <| slice region (Just 12) (Just 16) |>,
  phProfileClass       = <| parseProfileClass @$$ phProfileClassRegion |>,
  
  phColorSpaceRegion = <| slice region (Just 16) (Just 20) |>,
  phColorSpace       = <| parseColorSpace @$$ phColorSpaceRegion |>,

  phPCSRegion = <| slice region (Just 20) (Just 24) |>,
  phPCS       = <| parsePCS @$$ phPCSRegion |>,
  
  phDateTimeRegion = <| slice region (Just 24) (Just 36) |>,
  phDateTime       = <| parseDateTime @$$ phDateTimeRegion |>,

  -- profile signature "acsp"?

  phPrimaryPlatformRegion = <| slice region (Just 40) (Just 44) |>,
  phPrimaryPlatform       = <| parsePrimaryPlatform @$$ phPrimaryPlatformRegion |>,
  
  phProfileFlagsRegion = <| slice region (Just 44) (Just 48) |>,
  phProfileFlags       = <| parseProfileFlags @$$ phProfileFlagsRegion |>,
  
  phDeviceManufacturerRegion = <| slice region (Just 48) (Just 52) |>,
  phDeviceManufacturer       = <| parseDeviceManufacturer @$$ phDeviceManufacturerRegion |>,
  
  phDeviceModelRegion = <| slice region (Just 52) (Just 56) |>,
  phDeviceModel       = <| parseDeviceModel @$$ phDeviceModelRegion |>,
  
  phDeviceAttributesRegion = <| slice region (Just 56) (Just 64) |>,
  phDeviceAttributes       = <| parseDeviceAttributes @$$ phDeviceAttributesRegion |>,
  
  phRenderingIntentRegion = <| slice region (Just 64) (Just 68) |>,
  phRenderingIntent       = <| parseRenderingIntent @$$ phRenderingIntentRegion |>,

  phIlluminantRegion = <| slice region (Just 68) (Just 80) |>,
  phIlluminant       = <| parseIlluminant @$$ phIlluminantRegion |>,

  phCreatorRegion = <| slice region (Just 80) (Just 84) |>,
  phCreator       = <| parseCreator @$$ phCreatorRegion |>,

  phIdentifierRegion = <| slice region (Just 84) (Just 100) |>,
  phIdentifier       = <| parseIdentifier @$$ phIdentifierRegion |>,
  
  phSpectralPCSRegion = <| slice region (Just 100) (Just 104) |>,
  phSpectralPCS       = <| parseSpectralPCS @$$ phSpectralPCSRegion |>,
  
  phSpectralPCSWavelengthRegion = <| slice region (Just 104) (Just 110) |>,
  phSpectralPCSWavelength       = <| parseSpectralPCSWavelength @$$ phSpectralPCSWavelengthRegion |>,
  
  phBiSpectralPCSWavelengthRegion = <| slice region (Just 110) (Just 116) |>,
  phBiSpectralPCSWavelength       = <| parseBiSpectralPCSWavelength @$$ phBiSpectralPCSWavelengthRegion |>,
  
  phMCSSignatureRegion = <| slice region (Just 116) (Just 120) |>,
  phMCSSignature       = <| parseMCSSignature @$$ phMCSSignatureRegion |>,
  
  phProfileSubclassRegion = <| slice region (Just 120) (Just 124) |>,
  phProfileSubclass       = <| parseProfileSubclass @$$ phProfileSubclassRegion |>,

  -- reserved zeros?
}


type OptimalTagTable = {
  ttLen : Word32,
  ttTags : Vec<OptimalTag>,

  -- ttTagHeaders : Vec<OptimalTagHeader>,
  -- ttTagBodies : Vec<OptimalTagBody>,
}

mkOptimalTagTable : Region -> OptimalTagTable
mkOptimalTagTable region = {
  ttLenRegion = <| slice region (Just 0) (Just 4) |>,
  ttLen       = <| parseWord32 @$$ ttLenRegion |>,

  ttTagsRegion = <| slice region (Just 4) Nothing |>,
  ttTags       = generate ttLen <| mkOptimalTag ttTagsRegion |>,

  ---------------------------------------
  -- another method: headers, then bodies

  -- ttTagHeadersEnd     = {| 4 + ttLen * 12 |},
  -- ttTagHeadersRegion  = <| slice region (Just 4) (Just ttTagHeadersEnd) |>,
  -- ttTagHeadersRegions = generate ttLen 
  --   <| \i -> slice ttTagHeadersRegion (Just (i * 12)) (Just (i * 12 + 12)) |>,
  -- ttTagHeaders       = map ttTagHeadersRegions <| mkOptimalTagHeader |>,

  -- ttTagBodiesRegion = <| slice region (Just ttTagHeadersEnd) Nothing |>,
  -- ttTagBodies       = map ttTagHeaders <| mkOptimalTagBodyFromHeader ttTagBodiesRegion |>,
}


type OptimalTag = {
  tSig : Signature,
  tOffset : Word32,
  tSize : Word32,
  tElem : Tag
}

mkOptimalTag : Region -> Int -> OptimalTag
mkOptimalTag region idx = {
  tHeaderSize   = {| 12 |},
  tHeaderBegin  = {| fromIntegral idx * tHeaderSize |},
  tHeaderEnd    = {| tHeaderBegin + tHeaderSize |},
  tHeaderRegion = <| slice region (Just tHeaderBegin) (Just tHeaderEnd) |>,

  tSigRegion = <| slice tHeaderRegion (Just 0) (Just 4) |>,
  tSig       = <| parseSignature @$$ tSigRegion |>,

  tOffsetRegion = <| slice tHeaderRegion (Just 4) (Just 8) |>,
  tOffset       = <| parseWord32 @$$ tOffsetRegion |>,

  tSizeRegion = <| slice tHeaderRegion (Just 8) (Just 12) |>,
  tSize       = <| parseWord32 @$$ tSizeRegion |>,

  -- Our `region` elides the 132 bytes that comprise the ProfileHeader and tag
  -- table length, so we adjust our offsets accordingly
  tElemBegin  = {| fromIntegral tOffset - 132 |},
  tElemEnd    = {| tElemBegin + fromIntegral tSize |},
  tElemRegion = <| slice region (Just tElemBegin) (Just tElemEnd) |>,
  (tElem, _)  = <| parseTag @@!! tElemRegion |>,
}


type OptimalTagHeader = {
  thSig : Word32,
  thOffset : Word32,
  thSize : Word32,
}

mkOptimalTagHeader : Region -> OptimalTagHeader
mkOptimalTagHeader region = {
  thSigRegion = <| slice region Nothing (Just 4) |>,
  thSig       = <| parseWord32 @$$ thSigRegion |>,
  
  thOffsetRegion = <| slice region (Just 4) (Just 8) |>,
  thOffset       = <| parseWord32 @$$ thOffsetRegion |>,
  
  thSizeRegion = <| slice region (Just 8) (Just 12) |>,
  thSize       = <| parseWord32 @$$ thSizeRegion |>,
}


type OptimalTagBody = {
  tbBody : String
}

mkOptimalTagBodyFromHeader : Region -> OptimalTagHeader -> OptimalTagBody
mkOptimalTagBodyFromHeader region header = {
  offset = header.thOffset,
  size = header.thSize,
  -- Here, need access to top-level region (or current offset) in order to
  -- properly slice provided tag bodies region
  -- tbBodyRegion = <| sliceAbs region (Just offset) (Just (offset + size)) |>,
  tbBodyRegion = <| undefined region offset size |>,
  (tbBody, _) = <| parseString @@!! tbBodyRegion |>,
}

--------------------------------------------------------------------------------

-- getTagHeader : OptimalICC -> Int -> OptimalTagHeader
-- getTagHeader icc idx = {
--   table = icc.tagTable,
--   headers = table.ttTagHeaders,
--   header = index headers idx,
--   thSig = header.thSig,
--   thOffset = header.thOffset,
--   thSize = header.thSize,
-- }

-- getTagBody : OptimalICC -> Int -> OptimalTagBody
-- getTagBody icc idx = {
--   table = icc.tagTable,
--   bodies = table.ttTagBodies,
--   body = index bodies idx,
--   tbBody = body.tbBody,
-- }

|]

--------------------------------------------------------------------------------
-- PEAR

(@$$) :: Monad m => SRP m a -> Region -> PT m a
(@$$) srp region = v <$> appSRP srp region

(@!!) :: Monad m => SRP m a -> Region -> PT m (a, Region)
(@!!) srp region = first v <$> appSRP' srp region

(@@!!) :: Monad m => DRP m a -> Region -> PT m (a, Region)
(@@!!) = appDRP'

-- | A multipurpose region-manipulation combinator, functioning as `take`,
-- `drop`, or a combination
slice :: (Integral a, Monad m) => Region -> Maybe a -> Maybe a -> PT m Region
slice region beginM endM =
  case (beginM, endM) of
    (Nothing, Nothing) -> pure region
    (Just begin, Nothing) -> except (rDrop (fromIntegral begin) region)
    (Nothing, Just end) -> except (rTake (fromIntegral end) region)
    (Just begin, Just end) -> except $ rTake (fromIntegral end) region >>= rDrop (fromIntegral begin)

--------------------------------------------------------------------------------

newtype PreferredCMMType = PreferredCMMType Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parsePreferredCMMType :: Monad m => SRP m PreferredCMMType
parsePreferredCMMType =
  PreferredCMMType
    <$> signatures
      [ "ADBE",
        "ACMS",
        "aapl",
        "CCMS",
        "UCCM",
        "UCMS",
        "EFI ",
        "FF  ",
        "EXAC",
        "HCMM",
        "argl",
        "LgoS",
        "HDM ",
        "lcms",
        "RIMX",
        "DIMX",
        "KCMS",
        "MCML",
        "WCS ",
        "SIGN",
        "ONYX",
        "RGMS",
        "SICC",
        "TCMM",
        "32BT",
        "vivo",
        "WTG ",
        "zc00",
        "\NUL\NUL\NUL\NUL"
      ]

--------------------------------------------------------------------------------

data Version = Version
  { vMajor :: Word8,
    vMinor :: Word8,
    vBugfix :: Word8
  }
  deriving (Show, Debug m)

-- TODO: depends on sub-version
parseVersion :: Monad m => SRP m Version
parseVersion = mkPrimSRP width parse
  where
    width = 4
    parse contents =
      case contents of
        [chrToWord8 -> b0, chrToWord8 -> b1, _, _] ->
          pure (Version {vMajor = b0, vMinor = b1 `shiftR` 4, vBugfix = b1 .&. 0b1111})
        _ -> Left []

--------------------------------------------------------------------------------

newtype ProfileClass = ProfileClass Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseProfileClass :: Monad m => SRP m ProfileClass
parseProfileClass =
  ProfileClass
    <$> signatures
      [ "scnr",
        "mntr",
        "prtr",
        "link",
        "spac",
        "abst",
        "nmcl",
        "cenc",
        "mid ",
        "mlnk",
        "mvis"
      ]

--------------------------------------------------------------------------------

newtype ColorSpace = ColorSpace Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseColorSpace :: Monad m => SRP m ColorSpace
parseColorSpace = mkPrimSRP width parse
  where
    width = 4
    parse contents =
      case contents of
        "XYZ " -> cs contents
        "Lab " -> cs contents
        "Luv " -> cs contents
        "YCbr" -> cs contents
        "Yxy " -> cs contents
        "LMS " -> cs contents
        "RGB " -> cs contents
        "GRAY" -> cs contents
        "HSV " -> cs contents
        "HLS " -> cs contents
        "CMYK" -> cs contents
        "CMY " -> cs contents
        "2CLR" -> cs contents
        "3CLR" -> cs contents
        "4CLR" -> cs contents
        "5CLR" -> cs contents
        "6CLR" -> cs contents
        "7CLR" -> cs contents
        "8CLR" -> cs contents
        "9CLR" -> cs contents
        "ACLR" -> cs contents
        "BCLR" -> cs contents
        "CCLR" -> cs contents
        "DCLR" -> cs contents
        "ECLR" -> cs contents
        "FCLR" -> cs contents
        ['n', 'c', _, _] -> cs contents
        "\NUL\NUL\NUL\NUL" -> cs contents
        _ -> Left ["unknown color space:", contents]
    cs contents = ColorSpace <$> sig contents

--------------------------------------------------------------------------------

newtype PCS = PCS Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

-- TODO: depends on profile class and color space?
parsePCS :: Monad m => SRP m PCS
parsePCS = mkPrimSRP width parse
  where
    width = 4
    parse contents =
      case contents of
        "XYZ " -> pcs contents
        "Lab " -> pcs contents
        "\NUL\NUL\NUL\NUL" -> pcs contents
        _ -> Left ["unknown PCS:", contents]
    pcs contents = PCS <$> sig contents

--------------------------------------------------------------------------------

data DateTime = DateTime
  { dtYear :: Word16,
    dtMonth :: Word16,
    dtDay :: Word16,
    dtHour :: Word16,
    dtMinute :: Word16,
    dtSecond :: Word16
  }
  deriving (Show, Debug m)

parseDateTime :: Monad m => SRP m DateTime
parseDateTime = mkPrimSRP width parse
  where
    width = 12
    parse contents =
      case contents of
        [y0, y1, mo0, mo1, d0, d1, h0, h1, mi0, mi1, s0, s1] ->
          let dtYear = pack [y0, y1]
              dtMonth = pack [mo0, mo1]
              dtDay = pack [d0, d1]
              dtHour = pack [h0, h1]
              dtMinute = pack [mi0, mi1]
              dtSecond = pack [s0, s1]
           in pure (DateTime {..})
        _ -> error "impossible: width error"

--------------------------------------------------------------------------------

-- TODO: sum?
newtype PrimaryPlatform = PrimaryPlatform Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parsePrimaryPlatform :: Monad m => SRP m PrimaryPlatform
parsePrimaryPlatform =
  PrimaryPlatform
    <$> signatures
      [ "AAPL",
        "MSFT",
        "SGI ",
        "SUNW",
        "\NUL\NUL\NUL\NUL"
      ]

parsePrimaryPlatformPT :: Monad m => Region -> PT m PrimaryPlatform
parsePrimaryPlatformPT r = parsePrimaryPlatform @$$ r

--------------------------------------------------------------------------------

data ProfileFlags = ProfileFlags
  { pfEmbedded :: Bool,
    pfColorDataDependent :: Bool,
    pfMCSChannelsSubset :: Bool
  }
  deriving (Show, Debug m)

parseProfileFlags :: Monad m => SRP m ProfileFlags
parseProfileFlags = unpack <$> parseWord32
  where
    unpack word =
      let pfEmbedded = testBit word 0
          pfColorDataDependent = testBit word 1
          pfMCSChannelsSubset = testBit word 2
       in ProfileFlags {..}

--------------------------------------------------------------------------------

newtype DeviceManufacturer = DeviceManufacturer Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseDeviceManufacturer :: Monad m => SRP m DeviceManufacturer
parseDeviceManufacturer = DeviceManufacturer <$> parseSignature

--------------------------------------------------------------------------------

newtype DeviceModel = DeviceModel Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseDeviceModel :: Monad m => SRP m DeviceModel
parseDeviceModel = DeviceModel <$> parseSignature

--------------------------------------------------------------------------------

data DeviceAttributes = DeviceAttributes
  { daReflectiveOrTransparent :: ReflectiveOrTransparent,
    daGlossyOrMatte :: GlossyOrMatte,
    daPolarity :: Polarity,
    daMediaColor :: MediaColor,
    daPaperBased :: Bool,
    daTextured :: Bool,
    daIsotropic :: Bool,
    daSelfLuminous :: Bool
  }
  deriving (Show, Debug m)

-- reserved bits 8-32?
parseDeviceAttributes :: Monad m => SRP m DeviceAttributes
parseDeviceAttributes = mkPrimSRP width parse
  where
    width = 8
    parse contents =
      case map chrToWord8 contents of
        [byte, 0, 0, 0, _, _, _, _] ->
          let bit i zero one = if testBit byte i then one else zero
              daReflectiveOrTransparent = bit 0 Reflective Transparent
              daGlossyOrMatte = bit 1 Glossy Matte
              daPolarity = bit 2 Positive Negative
              daMediaColor = bit 3 Color BW
              daPaperBased = bit 4 True False
              daTextured = bit 5 False True
              daIsotropic = bit 6 True False
              daSelfLuminous = bit 7 False True
           in pure DeviceAttributes {..}
        [_, _, _, _, _, _, _, _] -> Left ["bad device attributes:", contents]
        _ -> error "impossible: bad width"

data ReflectiveOrTransparent = Reflective | Transparent
  deriving (Show, Debug m)

data GlossyOrMatte = Glossy | Matte
  deriving (Show, Debug m)

data Polarity = Positive | Negative
  deriving (Show, Debug m)

data MediaColor = Color | BW
  deriving (Show, Debug m)

--------------------------------------------------------------------------------

data RenderingIntent
  = Perceptual
  | MediaRelativeColorimetric
  | Saturation
  | ICCAbsoluteColorimetric
  deriving (Show, Debug m)

parseRenderingIntent :: Monad m => SRP m RenderingIntent
parseRenderingIntent = mkPrimSRP width parse
  where
    width = 4
    parse contents =
      case map chrToWord8 contents of
        [0, 0, 0, 0] -> pure Perceptual
        [0, 0, 0, 1] -> pure MediaRelativeColorimetric
        [0, 0, 0, 2] -> pure Saturation
        [0, 0, 0, 3] -> pure ICCAbsoluteColorimetric
        _ -> Left ["undefined rendering intent:", contents]

--------------------------------------------------------------------------------

newtype Illuminant = Illuminant XYZNumber
  deriving stock (Show)
  deriving anyclass (Debug m)

parseIlluminant :: Monad m => SRP m Illuminant
parseIlluminant = Illuminant <$> parseXYZ

--------------------------------------------------------------------------------

newtype Creator = Creator Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseCreator :: Monad m => SRP m Creator
parseCreator = Creator <$> parseSignature

--------------------------------------------------------------------------------

newtype Identifier = Identifier (Word32, Word32, Word32, Word32)
  deriving stock (Show)
  deriving anyclass (Debug m)

-- actually supposed to be an md5 sum...
parseIdentifier :: Monad m => SRP m Identifier
parseIdentifier =
  Identifier . flatten
    <$> pairSRPs parseWord32 (pairSRPs parseWord32 (pairSRPs parseWord32 parseWord32))
  where
    flatten (w0, (w1, (w2, w3))) = (w0, w1, w2, w3)

-- Is this version preferable?

parseIdentifier' :: Monad m => SRP m Identifier
parseIdentifier' = mkPrimSRP' 16 parseIdentifierPT

parseIdentifierPT :: Monad m => Region -> PT m Identifier
parseIdentifierPT r =
  do
    (w0, r1) <- parseWord32 @!! r
    (w1, r2) <- parseWord32 @!! r1
    (w2, r3) <- parseWord32 @!! r2
    (w3, _) <- parseWord32 @!! r3
    pure (Identifier (w0, w1, w2, w3))

--------------------------------------------------------------------------------

data SpectralPCS
  = Reflectance Word16
  | Transmission Word16
  | Radiant Word16
  | BiSpectral Word16
  | BiSpectralSparse Word16
  | Unused
  deriving (Show, Debug m)

parseSpectralPCS :: Monad m => SRP m SpectralPCS
parseSpectralPCS = mkPrimSRP width parse
  where
    width = 4
    parse contents =
      case contents of
        ['r', 's', b0, b1] -> pure (Reflectance (pack [b0, b1]))
        ['t', 's', b0, b1] -> pure (Transmission (pack [b0, b1]))
        ['e', 's', b0, b1] -> pure (Radiant (pack [b0, b1]))
        ['b', 's', b0, b1] -> pure (BiSpectral (pack [b0, b1]))
        ['s', 'm', b0, b1] -> pure (BiSpectralSparse (pack [b0, b1]))
        "\NUL\NUL\NUL\NUL" -> pure Unused
        [_, _, _, _] -> Left ["undefined spectral PCS:" <> contents]
        _ -> error "impossible: bad width"

--------------------------------------------------------------------------------

newtype SpectralPCSWavelength = SpectralPCSWavelength SpectralRange
  deriving stock (Show)
  deriving anyclass (Debug m)

parseSpectralPCSWavelength :: Monad m => SRP m SpectralPCSWavelength
parseSpectralPCSWavelength = SpectralPCSWavelength <$> parseSpectralRange

newtype BiSpectralPCSWavelength = BiSpectralPCSWavelength SpectralRange
  deriving stock (Show)
  deriving anyclass (Debug m)

parseBiSpectralPCSWavelength :: Monad m => SRP m BiSpectralPCSWavelength
parseBiSpectralPCSWavelength = BiSpectralPCSWavelength <$> parseSpectralRange

--------------------------------------------------------------------------------

newtype MCSSignature = MCSSignature Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseMCSSignature :: Monad m => SRP m MCSSignature
parseMCSSignature = MCSSignature <$> parseSignature

--------------------------------------------------------------------------------

newtype ProfileSubclass = ProfileSubclass Signature
  deriving stock (Show)
  deriving anyclass (Debug m)

parseProfileSubclass :: Monad m => SRP m ProfileSubclass
parseProfileSubclass = ProfileSubclass <$> parseSignature

--------------------------------------------------------------------------------

data Tag
  = MultiLocalizedUnicodeType String
  | MultiProcessElementsType String
  | SpectralViewingConditions String
  | XYZType String
  | GamutBoundaryDescription String
  | Other String
  deriving (Show, Debug m)

-- | Abbreviates the string data associated with each tag element
parseTag :: Monad m => DRP m Tag
parseTag = mkPrimDRP wc parse
  where
    wc = WC 4 MW_NoMax
    parse contents =
      case contents of
        ('m' : 'l' : 'u' : 'c' : rest) -> pure (MultiLocalizedUnicodeType (abbreviate rest), 0)
        ('m' : 'p' : 'e' : 't' : rest) -> pure (MultiProcessElementsType (abbreviate rest), 0)
        ('s' : 'v' : 'c' : 'n' : rest) -> pure (SpectralViewingConditions (abbreviate rest), 0)
        ('X' : 'Y' : 'Z' : ' ' : rest) -> pure (XYZType (abbreviate rest), 0)
        ('g' : 'b' : 'd' : ' ' : rest) -> pure (GamutBoundaryDescription (abbreviate rest), 0)
        (_ : _ : _ : _ : rest) -> pure (Other (abbreviate rest), 0)
        _ -> error "impossible: bad width"
    abbreviate xs =
      if length xs > 10
        then take 10 xs <> "..."
        else xs

--------------------------------------------------------------------------------
-- Debug

class MonadIO m => Debug m a where
  debug :: a -> m ()
  default debug :: Show a => a -> m ()
  debug = liftIO . print

instance MonadIO m => Debug m (OptimalICC m) where
  debug OptimalICC {..} =
    do
      debug profileHeader
      debug tagTable

instance MonadIO m => Debug m (OptimalProfileHeader m) where
  debug OptimalProfileHeader {..} =
    do
      debug phSize
      debug phPreferredCMMType
      debug phProfileVersion
      debug phProfileClass
      debug phColorSpace
      debug phPCS
      debug phDateTime
      debug phPrimaryPlatform
      debug phProfileFlags
      debug phDeviceManufacturer
      debug phDeviceModel
      debug phDeviceAttributes
      debug phRenderingIntent
      debug phIlluminant
      debug phCreator
      debug phIdentifier
      debug phSpectralPCS
      debug phSpectralPCSWavelength
      debug phBiSpectralPCSWavelength
      debug phMCSSignature
      debug phProfileClass

instance MonadIO m => Debug m Word32

instance MonadIO m => Debug m String

instance Debug m a => Debug m (Thunked m a) where
  debug t = force t >>= debug

instance Debug m a => Debug m (Vector m a) where
  debug (Vector ts) =
    do
      vs <- traverse force ts
      mapM_ debug vs

instance MonadIO m => Debug m (OptimalTagTable m) where
  debug OptimalTagTable {..} =
    do
      debug ttLen
      debug ttTags

instance MonadIO m => Debug m (OptimalTagHeader m) where
  debug OptimalTagHeader {..} =
    do
      debug thSig
      debug thOffset
      debug thSize

instance MonadIO m => Debug m (OptimalTagBody m) where
  debug OptimalTagBody {..} =
    do
      debug tbBody

instance MonadIO m => Debug m (OptimalTag m) where
  debug OptimalTag {..} =
    do
      debug tSig
      debug tOffset
      debug tSize
      debug tElem

--------------------------------------------------------------------------------
-- Underlying datatypes

newtype Signature = Signature (Char, Char, Char, Char)
  deriving stock (Show)
  deriving anyclass (Debug m)

parseSignature :: Monad m => SRP m Signature
parseSignature = mkPrimSRP width parse
  where
    width = 4
    parse contents =
      case contents of
        [c0, c1, c2, c3] -> pure (Signature (c0, c1, c2, c3))
        _ -> error "impossible: bad width"

sig :: [Char] -> Possibly Signature
sig contents =
  case contents of
    [c0, c1, c2, c3] -> Right (Signature (c0, c1, c2, c3))
    _ -> Left ["impossible: too many characters"]

signatures :: Monad m => [String] -> SRP m Signature
signatures allowed = mkPrimSRP 4 parse
  where
    parse contents =
      case contents of
        [c0, c1, c2, c3]
          | contents `elem` allowed -> Right (Signature (c0, c1, c2, c3))
          | otherwise ->
              Left $ ["unrecognized signature:", contents, "expected one of:"] <> allowed
        _ -> error "impossible: bad width"

data XYZNumber = XYZNumber
  { xyzX :: Int32,
    xyzY :: Int32,
    xyzZ :: Int32
  }
  deriving (Show, Debug m)

parseXYZ :: Monad m => SRP m XYZNumber
parseXYZ = mkPrimSRP width parse
  where
    width = 12
    parse contents =
      case contents of
        [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11] ->
          let xyzX = word32ToInt32 (pack [b0, b1, b2, b3])
              xyzY = word32ToInt32 (pack [b4, b5, b6, b7])
              xyzZ = word32ToInt32 (pack [b8, b9, b10, b11])
           in pure XYZNumber {..}
        _ -> error "impossible: bad width"

    word32ToInt32 :: Word32 -> Int32
    word32ToInt32 = fromIntegral

data SpectralRange = SpectralRange
  { srStart :: Float,
    srEnd :: Float,
    srStep :: Word16
  }
  deriving (Show, Debug m)

parseSpectralRange :: Monad m => SRP m SpectralRange
parseSpectralRange = mkPrimSRP width parse
  where
    width = 6
    parse contents =
      case contents of
        [b0, b1, b2, b3, b4, b5] ->
          let srStart = float (chrToWord8 b0) (chrToWord8 b1)
              srEnd = float (chrToWord8 b2) (chrToWord8 b3)
              srStep = pack [b4, b5]
           in pure SpectralRange {..}
        _ -> error "impossible: bad width"

--------------------------------------------------------------------------------
-- Base parsers

parseWord32 :: Monad m => SRP m Word32
parseWord32 = mkPrimSRP width (pure . parse)
  where
    width = 4
    parse = pack

parseWord64 :: Monad m => SRP m Word64
parseWord64 = mkPrimSRP width (pure . parse)
  where
    width = 8
    parse = pack

parseString :: Monad m => DRP m String
parseString = mkPrimDRP wc parse
  where
    wc = dynWC
    parse contents = pure (contents, 0)

--------------------------------------------------------------------------------
-- Util

chrToWord8 :: Char -> Word8
chrToWord8 = toEnum . fromEnum

pack :: (Bits a, Integral a) => [Char] -> a
pack = foldl (\acc (chrToWord8 -> byte) -> acc `shiftL` 8 .|. fromIntegral byte) 0

-- | Create a 16-bit float from two 8-bit words, most significant bits in first argument
--
-- not very clean, not very well-tested
float :: Word8 -> Word8 -> Float
float b0 b1 =
  let signed :: Num a => a -> a
      signed = if testBit b0 7 then negate else id

      expt :: Word8
      expt = (b0 .&. 0b01111100) `shiftR` 2

      mantissaWord :: Word16
      mantissaWord = ((fromIntegral b0 .&. 0b00000011) `shiftL` 8) .|. fromIntegral b1

      mantissa :: Ratio Integer
      mantissa = fromIntegral mantissaWord % 1024

      nan :: Float
      nan = castWord32ToFloat 0xffbfffff

      ratio :: Ratio Integer
      ratio =
        case expt of
          0 -> 1 % (2 ^ 14)
          _
            | fromIntegral expt - 15 < 0 -> traceShowId $ 1 % (2 ^ (15 - expt))
            | otherwise -> 2 ^ (expt - 15)
   in signed $
        case (expt, mantissa) of
          (0, _) -> fromRational (ratio * mantissa)
          (0b11111, 0) -> fromRational infinity
          (0b11111, _) -> nan
          _ -> fromRational (ratio * (1 + mantissa))
