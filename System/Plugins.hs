{-# LANGUAGE FlexibleInstances #-}
module System.Plugins where

import qualified BasicTypes
import qualified DynFlags
import qualified Encoding
import qualified FastString
import qualified GHC
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
           
           (flags, explicitPackages) <- liftIO $ Packages.initPackages flags
           liftIO $ putStrLn $ show $ map Module.packageIdString explicitPackages
           
           liftIO $ Linker.initDynLinker flags
           liftIO $ Linker.showLinkerState
           let packageId = Module.fsToPackageId (FastString.mkFastString "mtl-1.1.0.2")
           liftIO $ Linker.linkPackages flags [packageId]
           let packageId = Module.fsToPackageId (FastString.mkFastString "FruitTartInterface-1.0")
           liftIO $ Linker.linkPackages flags [packageId]
           
           liftIO $ Linker.showLinkerState
           
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

testSymbol :: String -> IO ()
testSymbol symbol = do
  maybePointer <- ObjLink.lookupSymbol symbol
  putStrLn $ symbol ++ " -> " ++ (show maybePointer)
