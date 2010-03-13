module System.Plugins (load) where

import qualified BasicTypes
import qualified DynFlags
import qualified Encoding
import qualified Exception
import qualified FastString
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths (libdir)
import qualified Linker
import qualified Module
import MonadUtils (liftIO)
import qualified Name
import qualified ObjLink
import qualified OccName
import qualified Packages
import qualified SrcLoc
import qualified Unique
import Unsafe.Coerce


load :: (String, String, String) -> IO (Maybe a)
load symbol@(packageName, moduleName, symbolName)
    = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
        GHC.runGhc (Just GHC.Paths.libdir) $ do
          flags <- GHC.getSessionDynFlags
          GHC.setSessionDynFlags flags
          
          (flags, _) <- liftIO $ Packages.initPackages flags
          
          liftIO $ Linker.initDynLinker flags
          let packageId = Module.fsToPackageId (FastString.mkFastString packageName)
          Exception.ghandle
            (\(GHC.CmdLineError _) -> do
               liftIO $ putStrLn $ "Unknown package " ++ packageName ++ "."
               return Nothing)
            (do
              liftIO $ Linker.linkPackages flags [packageId]
              
              Exception.ghandle
                (\(GHC.ProgramError string) -> do
                   if (hasPrefix string "Failed to load interface ")
                     then liftIO $ putStrLn $ "Unknown module " ++ moduleName
                                            ++ " in package " ++ packageName
                                            ++ "."
                     else liftIO $ putStrLn $ "Unknown symbol " ++ symbolName
                                            ++ " in module " ++ moduleName
                                            ++ " in package " ++ packageName
                                            ++ "."
                   return Nothing)
                (do
                  session <- GHC.getSession
                  let name = Name.mkExternalName
                               (Unique.mkBuiltinUnique 0)
                               (Module.mkModule packageId
                                                (Module.mkModuleName moduleName))
                               (OccName.mkVarOcc symbolName)
                               SrcLoc.noSrcSpan
                  result <- liftIO $ Linker.getHValue session name
                  return $ Just $ unsafeCoerce result))


encodeSymbol :: (String, String, String) -> String
encodeSymbol (packageName, moduleName, symbolName)
    = (Encoding.zEncodeString packageName)
      ++ "_"
      ++ (Encoding.zEncodeString moduleName)
      ++ "_"
      ++ (Encoding.zEncodeString symbolName)
      ++ "_closure"


hasPrefix :: String -> String -> Bool
hasPrefix string prefix = take (length prefix) string == prefix
