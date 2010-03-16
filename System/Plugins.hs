-- | The recommended interface, because it is safer (guaranteed not to crash as long
--   as modules have not been mis-installed somehow), is 'loadDynamic'.  For
--   versatility's sake, 'unsafeLoad' is provided as well, but caveat codor!
module System.Plugins (unsafeLoad, loadDynamic) where

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


-- | Resolves the specified symbol to any given type.  This means linking the package
--   containing it if it is not already linked, extracting the value of that symbol,
--   and returning that value.  Because a call is made to 'unsafeCoerce', the behavior
--   is unpredictable (most likely an immediate crash) if the symbol is not actually of
--   the expected type.  Because 'load' has no a priori way to know the type, you must
--   be certain to provide adequate type information in the caller, ie by giving a
--   type signature.
--   
--   Three error conditions are detected and handled nicely, returning
--   'Nothing':  The package does not exist; the package does not contain the given
--   module; or the module does not contain a symbol by the given name.
--   
--   As a limitation which may be relaxed in a future version, note that re-exports
--   are not chased; thus for example it is not possible to find the symbol
--   @base:Prelude.sum@, because that symbol is actually defined in @base:Data.List@.
unsafeLoad
    :: (String, String, String)
    -- ^ A tuple (@packageName@, @moduleName@, @symbolName@), specifying a symbol in
    --   a package installed somewhere in ghc's database.
    --   
    --   @packageName@ is a full package name including a version,
    --   ie @\"hello-1.0\"@; you can inspect these package names through
    --   @ghc-pkg@.
    --   
    --   @moduleName@ is a fully-qualified module name, ie @\"Hello\"@ or
    --   @\"Data.Dynamic\"@.
    --   
    --   @symbolName@ is an unqualified symbol name, ie @\"hello\"@.
    -> IO (Maybe a)
    -- ^ If the specified symbol is found, 'Just' its
    --   value.  Otherwise, 'Nothing'.
unsafeLoad symbol@(packageName, moduleName, symbolName)
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


-- | Resolves the specified symbol to a Dynamic.  This means first parsing the installed
--   .hi file for the package containing the symbol to verify that the symbol is in fact
--   a Dynamic, then, if it is, linking the package if it is not already linked,
--   extracting the value of that symbol, and returning that value.  Unlike 'load', this
--   function should be \"perfectly safe\", not crashing even if the symbol is not
--   actually of the expected type.
--   
--   Four error conditions are detected and handled nicely, returning
--   'Nothing':  The package does not exist; the package does not contain the given
--   module; the module does not contain a symbol by the given name; or the symbol's type
--   is not 'Dynamic'.
--   
--   As a limitation which may be relaxed in a future version, note that re-exports are
--   not chased; thus for example it is not possible to find the symbol
--   @base:Prelude.sum@, because that symbol is actually defined in @base:Data.List@.
--   (Also because that symbol is not a 'Dynamic'.)
loadDynamic
    :: (String, String, String)
    -- ^ A tuple (@packageName@, @moduleName@, @symbolName@), specifying a symbol in
    --   a package installed somewhere in ghc's database.
    --   
    --   @packageName@ is a full package name including a version,
    --   ie @\"hello-1.0\"@; you can inspect these package names through
    --   @ghc-pkg@.
    --   
    --   @moduleName@ is a fully-qualified module name, ie @\"Hello\"@ or
    --   @\"Data.Dynamic\"@.
    --   
    --   @symbolName@ is an unqualified symbol name, ie @\"hello\"@.
    -> IO (Maybe Dynamic)
    -- ^ If the specified symbol is found and of an appropriate type, 'Just' its
    --   value.  Otherwise, 'Nothing'.
loadDynamic symbol = do
  maybeInterfaceType <- symbolInterfaceType symbol
  case maybeInterfaceType of
    Nothing -> return Nothing
    Just interfaceType -> do
      let isDynamic = interfaceTypeIsDynamic interfaceType
      case isDynamic of
        False -> return Nothing
        True -> unsafeLoad symbol


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
