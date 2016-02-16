module CabalSupport (parseCabalFile,Unit(..),UnitName(..)) where

import Utils(ModName)

import Data.Maybe(maybeToList)
import System.FilePath((</>))

-- Interface to cabal.
import Distribution.PackageDescription.Parse(readPackageDescription)
import Distribution.Verbosity(silent)
import Distribution.PackageDescription
        ( GenericPackageDescription, PackageDescription(..)
        , Library(..), Executable(..), BuildInfo(..) )
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.ModuleName(ModuleName,components)




parseCabalFile :: FilePath -> IO [Unit]
parseCabalFile f = fmap findUnits (readPackageDescription silent f)


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
                   , unitPaths    = hsSourceDirs (libBuildInfo lib)
                   , unitModules  = map toMod (exposedModules lib)
                                                      -- other modules?
                   , unitFiles    = []
                   }

exeUnit :: Executable -> Unit
exeUnit exe = Unit { unitName    = UnitExecutable (exeName exe)
                   , unitPaths   = hsSourceDirs (buildInfo exe)
                   , unitModules = [] -- other modules?
                   , unitFiles   = case hsSourceDirs (buildInfo exe) of
                                     [] -> [ modulePath exe ]
                                     ds -> [ d </> modulePath exe | d <- ds ]
                   }

toMod :: ModuleName -> ModName
toMod m = case components m of
            [] -> error "Empty module name."
            xs -> (init xs, last xs)

findUnits :: GenericPackageDescription -> [Unit]
findUnits g = maybeToList (fmap libUnit (library pkg))  ++
                           fmap exeUnit (executables pkg)
  where
  pkg = flattenPackageDescription g -- we just ignore flags


