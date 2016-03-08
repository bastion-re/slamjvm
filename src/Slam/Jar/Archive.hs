-- | Functions for reading/writing JAR archives.
module Slam.Jar.Archive where

import Data.Map (keys)
import Data.Maybe

import Data.ByteString

import Path (Path, Rel, File, parseRelFile)
import Codec.Archive.Zip (withArchive, getEntries, getEntry,
                          unEntrySelector, mkEntrySelector, EntrySelector)

import Slam.Jar.Manifest (Manifest, runManifestParser)


listJar :: Path a File -> IO [Path Rel File]
listJar path = do
  files <- withArchive path (fmap keys getEntries)
  return $ fmap unEntrySelector files

readManifest :: Path a File -> IO ByteString
readManifest path = withArchive path (mf >>= getEntry) where
  mf = parseRelFile "META-INF/MANIFEST.MF" >>= mkEntrySelector

parseManifest :: Path a File -> IO (Maybe Manifest)
parseManifest path = do
  manifest <- readManifest path
  return $ runManifestParser manifest
