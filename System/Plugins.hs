{-# LANGUAGE FlexibleInstances #-}
module System.Plugins where

import qualified BasicTypes
import qualified DynFlags
import qualified Encoding
import qualified FastString
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths (libdir)
import qualified Linker
import qualified Module
import qualified ObjLink
import qualified Packages
import MonadUtils


main :: IO ()
main = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
         GHC.runGhc (Just GHC.Paths.libdir) $ do
           flags <- GHC.getSessionDynFlags
           GHC.setSessionDynFlags flags
           
           (flags, _) <- liftIO $ Packages.initPackages flags
           
           liftIO $ Linker.initDynLinker flags
           let packageId = Module.fsToPackageId (FastString.mkFastString "hello-1.0")
           liftIO $ Linker.linkPackages flags [packageId]
           
           liftIO $ testSymbol $
                           "__stginit_"
                           ++ (Encoding.zEncodeString "array-0.3.0.0")
                           ++ "_"
                           ++ (Encoding.zEncodeString "Data.Array")
           liftIO $ testSymbol $
                           (Encoding.zEncodeString "array-0.3.0.0")
                           ++ "_"
                           ++ (Encoding.zEncodeString "Data.Array.Base")
                           ++ "_"
                           ++ (Encoding.zEncodeString "array")
                           ++ "_closure"
           liftIO $ testSymbol $ "__stginit_base_DataziBits_"
           liftIO $ testSymbol $ "__stginit_mtlzm1zi1zi0zi2_ControlziMonadziCont"
           liftIO $ testSymbol $ "FruitTartInterfacezm1zi0_NetworkziFruitTartziBase_canonicalURL_closure"
           liftIO $ testSymbol $ "FruitTartInterfacezm1zi0_NetworkziFruitTartziBase_canonicalURL_closure"

           maybePointer <- ObjLink.lookupSymbol $
                           (Encoding.zEncodeString "hello-1.0")
                           ++ "_"
                           ++ (Encoding.zEncodeString "Hello")
                           ++ "_"
                           ++ (Encoding.zEncodeString "hello")
                           ++ "_closure"
           case maybePointer of
             Nothing -> putStrLn $ "Unable to load hello."
             Just pointer -> do
               case GHC.Exts.addrToHValue# pointer of
                 (# hValue #) -> putStrLn $ hValue
           putStrLn "foom"

testSymbol :: String -> IO ()
testSymbol symbol = do
  maybePointer <- ObjLink.lookupSymbol symbol
  putStrLn $ symbol ++ " -> " ++ (show maybePointer)
