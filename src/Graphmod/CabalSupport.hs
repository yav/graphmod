{-# language CPP #-}
{-# language DataKinds #-}

module Graphmod.CabalSupport (parseCabalFile,Unit(..),UnitName(..)) where

import Graphmod.Utils(ModName,fromHierarchy)

import Data.Maybe(maybeToList)
import System.FilePath((</>))

-- Interface to cabal.
import Distribution.Verbosity(silent)
import Distribution.PackageDescription
        ( GenericPackageDescription, PackageDescription(..)
        , Library(..), Executable(..), BuildInfo(..) )
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.ModuleName(ModuleName,components)

#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path (SymbolicPathX, getSymbolicPath, makeSymbolicPath)
#elif MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (SymbolicPath, PackageDir, SourceDir, getSymbolicPath)
#endif

#if MIN_VERSION_Cabal(2,0,0)

#if MIN_VERSION_Cabal(3,8,1)
import Distribution.Simple.PackageDescription(readGenericPackageDescription)
#elif MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec(readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse(readGenericPackageDescription)
#endif

import Distribution.Types.UnqualComponentName (UnqualComponentName)

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.Pretty (prettyShow)

pretty :: UnqualComponentName -> String
pretty = prettyShow
#else
import Distribution.Text (disp)
import Text.PrettyPrint (render)

pretty :: UnqualComponentName -> String
pretty = render . disp
#endif


#else
import Distribution.PackageDescription.Parse(readPackageDescription)
import Distribution.Verbosity (Verbosity)

readGenericPackageDescription :: Verbosity -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription

pretty :: String -> String
pretty = id
#endif

-- Note that this isn't nested under the above #if because we need
-- the backwards-compatible version to be available for all Cabal
-- versions prior to 3.6
#if MIN_VERSION_Cabal(3,14,0)
sourceDirToFilePath :: SymbolicPathX allowAbsolute pkg src -> FilePath
sourceDirToFilePath = getSymbolicPath
#elif MIN_VERSION_Cabal(3,6,0)
sourceDirToFilePath :: SymbolicPath PackageDir SourceDir -> FilePath
sourceDirToFilePath = getSymbolicPath
#else
sourceDirToFilePath :: FilePath -> FilePath
sourceDirToFilePath = id
#endif

parseCabalFile :: FilePath -> IO [Unit]
parseCabalFile f = fmap findUnits $
#if MIN_VERSION_Cabal(3,14,0)
  readGenericPackageDescription silent Nothing $ makeSymbolicPath f
#else
  readGenericPackageDescription silent f
#endif

-- | This is our abstraction for something in a cabal file.
data Unit = Unit
  { unitName    :: UnitName
  , unitPaths   :: [FilePath]
  , unitModules :: [ModName]
  , unitFiles   :: [FilePath]
  } deriving Show

data UnitName = UnitLibrary | UnitExecutable String
                deriving Show


libUnit :: Library -> Unit
libUnit lib = Unit { unitName     = UnitLibrary
                   , unitPaths    = sourceDirToFilePath <$> hsSourceDirs (libBuildInfo lib)
                   , unitModules  = map toMod (exposedModules lib)
                                                      -- other modules?
                   , unitFiles    = []
                   }

exeUnit :: Executable -> Unit
exeUnit exe = Unit { unitName    = UnitExecutable (pretty $ exeName exe)
                   , unitPaths   = sourceDirToFilePath <$> hsSourceDirs (buildInfo exe)
                   , unitModules = [] -- other modules?
                   , unitFiles   =
#if MIN_VERSION_Cabal(3,14,0)
                    let p = sourceDirToFilePath $ modulePath exe
#else
                    let p = modulePath exe
#endif
                    in case hsSourceDirs (buildInfo exe) of
                      [] -> [ p ]
                      ds -> [ sourceDirToFilePath d </> p | d <- ds ]
                   }

toMod :: ModuleName -> ModName
toMod m = case components m of
            [] -> error "Empty module name."
            xs -> (fromHierarchy (init xs), last xs)

findUnits :: GenericPackageDescription -> [Unit]
findUnits g = maybeToList (fmap libUnit (library pkg))  ++
                           fmap exeUnit (executables pkg)
  where
  pkg = flattenPackageDescription g -- we just ignore flags
