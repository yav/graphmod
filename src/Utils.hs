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

import Language.Haskell.Lexer(lexerPass1,Token(..))

import Data.Maybe(catMaybes)
import Data.List(intersperse,isPrefixOf)
import System.Directory(doesFileExist)
import System.FilePath

import Debug.Trace

-- | Get the imports of a file.
parseFile          :: FilePath -> IO (ModName,[ModName])
parseFile f         = (debug . parseString . get_text) `fmap` readFile f
  where get_text txt = if takeExtension f == ".lhs" then delit txt else txt
        debug z@(x,y) = z -- trace ("imports of " ++ show x ++ show y) z

-- | Get the imports from a string that represents a program.
parseString        :: String -> (ModName,[ModName])
parseString         = parse . debug . lexerPass1
  where debug xs = xs -- trace (unlines $ "tokens: " : map show xs) xs


isImp (_ : (Conid, (_,x)) : xs)   = Just (x,xs)
isImp (_ : (Qconid, (_,x)) : xs)  = Just (x,xs)
-- isImp (_ : (Specialid,_) : (Conid, (_,x)) : xs)   = Just (x,xs)
-- isImp (_ : (Specialid,_) : (Qconid, (_,x)) : xs)  = Just (x,xs)
isImp (_ : (Varid,_) : (Conid, (_,x)) : xs)   = Just (x,xs)
isImp (_ : (Varid,_) : (Qconid, (_,x)) : xs)  = Just (x,xs)
isImp _ = Nothing

-- parse xs | trace (show (take 10 xs)) False = undefined
parse (_ : (Reservedid,(_,"module")) : (_,(_,m)) : is) =
                                                  (splitModName m,imports is)
parse is = {-trace ("Defaulting to Main: " ++ show (take 10 is))-}
           (([],"Main"),imports is)

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


delit :: String -> String
delit txt = unlines $ bird $ lines txt
  where
  bird (('>' : cs) : ls)  = (' ' : cs) : bird ls
  bird (l : ls)
    | "\\begin{code}" `isPrefixOf` l  = in_code ls
    | otherwise                       = bird ls
  bird []                             = []

  in_code (l : ls)
    | "\\end{code}" `isPrefixOf` l    = bird ls
    | otherwise                       = l : in_code ls
  in_code []                          = []    -- unterminated code...



