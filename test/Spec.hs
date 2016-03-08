import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, assertEqual)

import Control.Monad
import Data.Maybe (isJust, fromJust)

import Data.ByteString (null)
import System.IO.Unsafe (unsafePerformIO)

import Path (parseRelFile, Path, Rel, File)

import Slam.Jar.Archive (listJar, readManifest, parseManifest)
import Slam.Jar.Manifest

main :: IO ()
main = defaultMain tests

jarTestFiles :: IO [Path Rel File]
jarTestFiles = mapM parseRelFile ["com/apple/concurrent/Dispatch$Priority.class",
   "com/apple/concurrent/Dispatch.class",
   "com/apple/eawt/Application.class",
   "com/apple/eawt/ApplicationAdapter.class",
   "com/apple/eawt/ApplicationBeanInfo.class",
   "com/apple/eawt/ApplicationEvent.class",
   "com/apple/eawt/ApplicationListener.class",
   "com/apple/eawt/CocoaComponent.class",
   "com/apple/eawt/event/GestureAdapter.class",
   "com/apple/eawt/event/GestureEvent.class",
   "com/apple/eawt/event/GestureListener.class",
   "com/apple/eawt/event/GesturePhaseEvent.class",
   "com/apple/eawt/event/GesturePhaseListener.class",
   "com/apple/eawt/event/GestureUtilities.class",
   "com/apple/eawt/event/MagnificationEvent.class",
   "com/apple/eawt/event/MagnificationListener.class",
   "com/apple/eawt/event/RotationEvent.class",
   "com/apple/eawt/event/RotationListener.class",
   "com/apple/eawt/event/SwipeEvent.class",
   "com/apple/eawt/event/SwipeListener.class",
   "com/apple/eio/FileManager.class",
   "META-INF/INDEX.LIST",
   "META-INF/MANIFEST.MF"]

zipFile :: IO (Path Rel File)
zipFile = parseRelFile "test/apple.jar"

testJarArchive = testGroup "Slam.Jar.Archive" [
  testCase "list contents of jar" $ assertEqual "jar files"
    (unsafePerformIO (zipFile >>= listJar))
    (unsafePerformIO jarTestFiles),
  testCase "read, but not parse, the manifest" $
   assertBool "manifest file was read" $
   unsafePerformIO $
   fmap (not . Data.ByteString.null) (zipFile >>= readManifest)
  ]

appleManifest = Manifest [(Unknown "Ant-Version","Apache Ant 1.7.1"),
                          (CreatedBy,"16.3-b01-eng (Apple Inc.)"),
                          (ManifestVersion,"1.0")]

testJarManifest = testGroup "Slam.Jar.Manifest" [
  testCase "parse a jar manifest" $ assertBool "Parsed successfully" $
  unsafePerformIO $ fmap (isJust . runManifestParser) (zipFile >>= readManifest)
  ]

tests = [ testJarArchive,
          testJarManifest ]
