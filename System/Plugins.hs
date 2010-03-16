module System.Plugins (load, loadDynamic) where

import Data.Dynamic
import Data.IORef
import Data.List
import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

import qualified BasicTypes
import qualified DynFlags
import qualified Encoding
import qualified Exception
import qualified FastString
import qualified GHC
import qualified GHC.Exts
import qualified GHC.Paths (libdir)
import qualified HscTypes
import qualified IfaceSyn
import qualified IfaceType
import qualified IOEnv
import qualified Linker
import qualified LoadIface
import qualified Maybes
import qualified Module
import MonadUtils (liftIO)
import qualified Name
import qualified ObjLink
import qualified OccName
import qualified Outputable
import qualified PackageConfig
import qualified Packages
import qualified TcRnTypes
import qualified SrcLoc
import qualified UniqSupply
import qualified Unique


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


loadDynamic :: (String, String, String) -> IO (Maybe Dynamic)
loadDynamic symbol = do
  maybeInterfaceType <- symbolInterfaceType symbol
  case maybeInterfaceType of
    Nothing -> return Nothing
    Just interfaceType -> do
      let isDynamic = interfaceTypeIsDynamic interfaceType
      case isDynamic of
        False -> return Nothing
        True -> load symbol


symbolInterfaceType :: (String, String, String) -> IO (Maybe IfaceType.IfaceType)
symbolInterfaceType (packageName, moduleName, symbolName)
    = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
        GHC.runGhc (Just GHC.Paths.libdir) $ do
          flags <- GHC.getSessionDynFlags
          GHC.setSessionDynFlags flags
          
          (flags, _) <- liftIO $ Packages.initPackages flags
          
          uniqueSupply <- liftIO $ UniqSupply.mkSplitUniqSupply 'a'
          uniqueSupplyIORef <- liftIO $ newIORef uniqueSupply
          hscEnv <- GHC.getSession
          let packageState = DynFlags.pkgState flags
              packageId = Module.fsToPackageId (FastString.mkFastString packageName)
          case Packages.lookupPackage (Packages.pkgIdMap packageState) packageId of
            Nothing -> do
              liftIO $ putStrLn $ "Unknown package " ++ packageName ++ "."
              return Nothing
            Just packageConfig -> do
              let installedPackageInfo
                    = PackageConfig.packageConfigToInstalledPackageInfo packageConfig
                  module' = Module.mkModule packageId $ Module.mkModuleName moduleName
                  libraryDirs = PackageConfig.libraryDirs installedPackageInfo
                  interfacePathWithinLibraryDir = moduleNameToInterfacePath moduleName
                  visitLibraryDir (libraryDir : moreLibraryDirs) = (do
                    let interfacePath = libraryDir ++ "/" ++ interfacePathWithinLibraryDir
                        environment = TcRnTypes.Env {
                                        TcRnTypes.env_top = hscEnv,
                                        TcRnTypes.env_us = uniqueSupplyIORef,
                                        TcRnTypes.env_gbl = (),
                                        TcRnTypes.env_lcl = ()
                                      }
                    errorOrModuleInterface
                      <- liftIO $ IOEnv.runIOEnv environment
                         $ LoadIface.readIface module' interfacePath False
                    case errorOrModuleInterface of
                      Maybes.Failed message -> visitLibraryDir moreLibraryDirs
                      Maybes.Succeeded moduleInterface -> do
                        let variableName = OccName.mkVarOcc symbolName
                            declarations = HscTypes.mi_decls moduleInterface
                            visitDeclaration ((_, declaration) : moreDeclarations) = (do
                              case declaration of
                                IfaceSyn.IfaceId { } -> do
                                  if IfaceSyn.ifName declaration == variableName
                                    then return $ Just $ IfaceSyn.ifType declaration
                                    else visitDeclaration moreDeclarations
                                _ -> visitDeclaration moreDeclarations)
                            visitDeclaration [] = do
                              liftIO $ putStrLn $ "Unknown symbol " ++ symbolName
                                                  ++ " in module " ++ moduleName
                                                  ++ " in package " ++ packageName
                                                  ++ "."
                              return Nothing
                        visitDeclaration declarations)
                  visitLibraryDir [] = do
                    liftIO $ putStrLn $ "Unknown module " ++ moduleName
                                        ++ " in package " ++ packageName
                                        ++ "."
                    return Nothing
              visitLibraryDir libraryDirs


interfaceTypeToString :: IfaceType.IfaceType -> String
interfaceTypeToString ifaceType
    = Outputable.showSDoc $ IfaceType.pprIfaceType ifaceType


interfaceTypeIsDynamic :: IfaceType.IfaceType -> Bool
interfaceTypeIsDynamic (IfaceType.IfaceTyConApp constructor arguments)
    = let interfaceTypeConstructorIsDynamic (IfaceType.IfaceTc name)
              = let occName = Name.nameOccName name
                    typeName = OccName.occNameString occName
                    module' = Name.nameModule name
                    packageId = Module.modulePackageId module'
                    packageName = Module.packageIdString packageId
                    moduleName = Module.moduleNameString $ Module.moduleName module'
                in (packageName == "base")
                   && (moduleName == "Data.Dynamic")
                   && (typeName == "Dynamic")
          interfaceTypeConstructorIsDynamic _ = False
      in (length arguments == 0) && (interfaceTypeConstructorIsDynamic constructor)
interfaceTypeIsDynamic _ = False


encodeSymbol :: (String, String, String) -> String
encodeSymbol (packageName, moduleName, symbolName)
    = (Encoding.zEncodeString packageName)
      ++ "_"
      ++ (Encoding.zEncodeString moduleName)
      ++ "_"
      ++ (Encoding.zEncodeString symbolName)
      ++ "_closure"


moduleNameToInterfacePath :: String -> String
moduleNameToInterfacePath moduleName = 
    let translateCharacter '.' = '/'
        translateCharacter c = c
    in (map translateCharacter moduleName) ++ ".hi"


hasPrefix :: String -> String -> Bool
hasPrefix string prefix = take (length prefix) string == prefix
