module Utils
  ( parseFile
  , parseString
  , ModName
  , splitModName
  , joinModName
  , relPaths
  , modToFile
  , suffixes
  ) where

import HsLexerPass1(lexerPass1)
import HsTokens

import Data.Maybe(catMaybes)
import Data.List(intersperse)
import System.Directory(doesFileExist)
import System.FilePath

import Debug.Trace

-- | Get the imports of a file.
parseFile          :: FilePath -> IO (ModName,[ModName])
parseFile f         = parseString `fmap` readFile f

-- | Get the imports from a string that represents a program.
parseString        :: String -> (ModName,[ModName])
parseString         = parse . lexerPass1


isImp (_ : (Conid, (_,x)) : xs)   = Just (x,xs)
isImp (_ : (Qconid, (_,x)) : xs)  = Just (x,xs)
isImp (_ : (Specialid,_) : (Conid, (_,x)) : xs)   = Just (x,xs)
isImp (_ : (Specialid,_) : (Qconid, (_,x)) : xs)  = Just (x,xs)
isImp _ = Nothing

-- parse xs | trace (show (take 10 xs)) False = undefined
parse (_ : (Reservedid,(_,"module")) : (_,(_,m)) : is) =
                                                  (splitModName m,imports is)
parse is = (([],"Main"),imports is)

imports xs          = case isImp $ snd $ break (("import" ==) . snd . snd) xs of
                        Just (x,xs) -> splitModName x : imports xs
                        _           -> []

-- | A hirarchical module name.
type ModName        = ([String],String)

-- | Convert a string name into a hirarchical name.
splitModName       :: String -> ModName
splitModName xs     = case break ('.'==) xs of
                        (xs,_:ys)  -> let (as,bs) = splitModName ys
                                   in (xs:as,bs)
                        _ -> ([],xs)

joinModName        :: ModName -> String
joinModName (xs,y)  = concat $ intersperse "." (xs ++ [y])

-- | The files in which a module moght reside.
relPaths           :: ModName -> [FilePath]
relPaths (xs,y)     = [ prefix ++ suffix | suffix <- suffixes ]
  where prefix      = foldr (</>) y xs

suffixes    = [".hs",".lhs"]

-- | The files in which a module might reside.
-- We report only files that exist.
modToFile          :: [FilePath] -> ModName -> IO [FilePath]
modToFile dirs m    = catMaybes `fmap` mapM check paths
  where
  paths             = [ d </> r | d <- dirs, r <- relPaths m ]
  check p           = do x <- doesFileExist p
                         return (if x then Just p else Nothing)

