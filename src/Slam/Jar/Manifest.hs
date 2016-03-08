{-# LANGUAGE OverloadedStrings #-}

-- | https://docs.oracle.com/javase/8/docs/technotes/guides/jar/jar.html#JAR_Manifest
module Slam.Jar.Manifest where

import Control.Applicative ((<|>))
import Control.Monad (void)

import Data.ByteString (ByteString())
import Data.ByteString.Char8 (pack, unpack)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8 (char, isDigit_w8, char8)

import Data.Binary

runManifestParser :: ByteString -> Maybe Manifest
runManifestParser x = maybeResult $ parse parseManifest x

type ManifestKeyVal = (ManifestAttribute, String)
type ManifestEntryKeyVal = (ManifestEntryAttribute, String)
data Section = Section String [ManifestEntryKeyVal]
               deriving (Show)
data Manifest = Manifest [ManifestKeyVal] [Section]
                deriving (Show)


data ManifestAttribute =
  ManifestVersion |
  CreatedBy |
  SignatureVersion |
  ClassPath |
  MainClass |
  ExtensionName |
  ImplementationTitle |
  ImplementationVersion |
  ImplementationVendor |
  SpecificationTitle |
  SpecificationVersion |
  SpecificationVendor |
  Sealed |
  Unknown String

instance Show ManifestAttribute where
  show ManifestVersion = "Manifest-Version"
  show CreatedBy = "Created-By"
  show SignatureVersion = "Signature-Version"
  show ClassPath = "Class-Path"
  show MainClass = "Main-Class"
  show ExtensionName = "Extension-Name"
  show ImplementationTitle = "Implementation-Title"
  show ImplementationVersion = "Implementation-Version"
  show ImplementationVendor = "Implementation-Vendor"
  show SpecificationTitle = "Specification-Title"
  show SpecificationVersion = "Specification-Version"
  show SpecificationVendor = "Specification-Vendor"
  show Sealed = "Sealed"
  show (Unknown x) = x

data ManifestEntryAttribute =
  ContentType |
  JavaBean |
  Digest String String |
  Magic |
  UnknownEntry String

instance Show ManifestEntryAttribute where
  show ContentType = "Content-Type"
  show JavaBean = "Java-Bean"
  show (Digest xs ys) = xs ++ "-Digest-" ++ ys
  show Magic = "Magic"
  show (UnknownEntry x) = x

parseManifest :: Parser Manifest
parseManifest = do
  version <- parseVersionInfo
  mainAttrs <- many1 parseManifestKeyValue
  sections <- many' parseIndividualSection
  skipNewLine
  return $ Manifest (mainAttrs ++ [version]) sections
  <?> "parseManifest"

parseVersionInfo :: Parser ManifestKeyVal
parseVersionInfo = do
  _ <- string "Manifest-Version"
  parseDef
  version <- parseVersionNum
  skipNewLine
  return (ManifestVersion, version)
  <?> "parseVersionInfo" where
    parseVersionNum = fmap (fmap word8ToChar) (many1 (digit <|> char8 '.'))
    digit = satisfy isDigit_w8

parseIndividualSection :: Parser Section
parseIndividualSection = do
  _ <- string "Name"
  parseDef
  name <- parseManifestValue
  skipNewLine
  attrs <- many' parseManifestEntryKeyValue
  return $ Section name attrs
  <?> "parseIndividualSection"

parseManifestKeyValue :: Parser ManifestKeyVal
parseManifestKeyValue = do
  k <- parseManifestKeyName
  parseDef
  v <- parseManifestValue
  return (k, v)
  <?> "parseManifestKeyValue"

parseManifestEntryKeyValue :: Parser ManifestEntryKeyVal
parseManifestEntryKeyValue = do
  k <- parseManifestEntryKeyName
  parseDef
  v <- parseManifestValue
  return (k, v)
  <?> "parseManifestEntryKeyValue"

parseManifestKeyName :: Parser ManifestAttribute
parseManifestKeyName = do
  parsed <- fmap pack parseManifestKey
  return $ case parsed of
    "Manifest-Version" -> ManifestVersion
    "Created-By" -> CreatedBy
    "Signature-Version" -> SignatureVersion
    "Class-Path" -> ClassPath
    "Main-Class" -> MainClass
    "Extension-Name" -> ExtensionName
    "Implementation-Title" -> ImplementationTitle
    "Implementation-Version" -> ImplementationVersion
    "Implementation-Vendor" -> ImplementationVendor
    "Specification-Title" -> SpecificationTitle
    "Specification-Version" -> SpecificationVersion
    "Specification-Vendor" -> SpecificationVendor
    "Sealed" -> Sealed
    "Ant-Version" -> Unknown "Ant-Version"
    x -> Unknown (unpack x)
  <?> "parseManifestKeyName"

parseManifestEntryKeyName :: Parser ManifestEntryAttribute
parseManifestEntryKeyName = do
  parsed <- fmap pack parseManifestKey
  return $ case parsed of
    "Content-Type" -> ContentType
    "Java-Bean" -> JavaBean
    "Magic" -> Magic
    "Digest" -> Digest "" "" -- yeah fuck off
    x -> UnknownEntry (unpack x)
  <?> "parseManifestEntryKeyName"

parseManifestKey :: Parser String
parseManifestKey = many' (parseAlphaNum <|> parseHeaderChar)
  <?> "parseManifestKey"

parseManifestValue :: Parser String
parseManifestValue = do
  x <- many1 parseOtherChar
  skipNewLine
  xs <- many' parseContinuation
  return $ x ++ (concat xs)
  <?> "parseManifestValue"

parseContinuation :: Parser String
parseContinuation = do
  skipSpace
  x <- many' parseOtherChar
  skipNewLine
  return x
  <?> "parseContinuation"

parseDef :: Parser ()
parseDef = void (many' skipSpace >> char ':' >> many' skipSpace)
  <?> "parseDef"

parseHeaderChar :: Parser Char
parseHeaderChar = satisfyChar (inClass "-_a-zA-Z0-9")
  <?> "parseHeaderChar"

parseAlphaNum :: Parser Char
parseAlphaNum = satisfyChar (inClass "a-zA-Z0-9")
  <?> "parseAlphaNum"

parseOtherChar :: Parser Char
parseOtherChar = satisfyChar (notInClass "\x00\r\n")
  <?> "parseOtherchar"

skipSpace :: Parser ()
skipSpace = void (char ' ') <?> "skipSpace"

skipNewLine :: Parser ()
skipNewLine = void (string "\r\n" <|>
                     string "\n" <|>
                     string "\r") <?> "skipNewLine"

word8ToChar :: Word8 -> Char
word8ToChar = toEnum . fromEnum

satisfyChar :: (Word8 -> Bool) -> Parser Char
satisfyChar p = fmap word8ToChar (satisfy p) <?> "satisfyChar"
