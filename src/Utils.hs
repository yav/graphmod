module Utils
  ( parseFile
  , parseString
  , Qualifier
  , Import(..)
  , ImpType(..)
  , splitQualifier
  , ModName
  , splitModName
  , joinModName
  , relPaths
  , modToFile
  , suffixes
  ) where

import Language.Haskell.Lexer(lexerPass0,Token(..),PosToken,line)

import Control.Monad(mplus)
import Control.Exception(evaluate)
import Data.Maybe(catMaybes)
import Data.List(intersperse,isPrefixOf)
import System.Directory(doesFileExist)
import System.FilePath

data Import = Import { impMod :: ModName, impType :: ImpType }
                deriving Show

data ImpType = NormalImp | SourceImp
                deriving (Show,Eq,Ord)

-- | Get the imports of a file.
parseFile          :: FilePath -> IO (ModName,[Import])
parseFile f =
  do (modName, imps) <- (parseString . get_text) `fmap` readFile f
     _ <- evaluate (length imps) -- this is here so that the file gets closed
     if ext == ".imports"
       then return (splitModName (takeBaseName f), imps)
       else return (modName, imps)


  where get_text txt = if ext == ".lhs" then delit txt else txt
        ext          = takeExtension f

-- | Get the imports from a string that represents a program.
parseString        :: String -> (ModName,[Import])
parseString         = parse . dropApproxCPP . dropComments . lexerPass0


-- | Drop comments, but keep {-# SOURCE #-} pragmas.
dropComments :: [PosToken] -> [PosToken]
dropComments = filter (not . skip)
  where
  skip (t, (_,txt))
    |  t == Whitespace
    || t == Commentstart
    || t == Comment
    || t == LiterateComment = True
    |  t == NestedComment   = not (isSourcePragma txt)
    | otherwise             = False


isSourcePragma :: String -> Bool
isSourcePragma txt = case words txt of
                       ["{-#", "SOURCE", "#-}"] -> True
                       _                        -> False


dropApproxCPP :: [PosToken] -> [PosToken]

 -- this is some artifact of the lexer
dropApproxCPP ((_, (_,"")) : more) = dropApproxCPP more

dropApproxCPP ((Varsym, (_,"#")) : (_, (pos,tok)) : more)
  | tok `elem` [ "if", "ifdef", "ifndef" ] = dropToEndif more
  | tok `elem` [ "include", "define", "undef" ] = dropToEOL more
  where
  dropToEndif ((Varsym, (_,"#")) : (_, (_,"endif")) : rest)
                         = dropApproxCPP rest
  dropToEndif (_ : rest) = dropToEndif rest
  dropToEndif []         = []

  dropToEOL ((_, (pos1,_)) : rest)
    | line pos == line pos1 = dropToEOL rest
  dropToEOL xs  = dropApproxCPP xs

dropApproxCPP (x : xs) = x : dropApproxCPP xs
dropApproxCPP []       = []


-- 'import' maybe_src maybe_safe optqualified maybe_pkg modid
--                                                        maybeas maybeimpspec
isImp :: [PosToken] -> Maybe (Import, [PosToken])
isImp ts = attempt (1::Int) (drop 1 ts)
  where
  attempt n toks
    -- import safe qualified "package" ModId
    | n > 4     = Nothing
    | otherwise = mplus (isMod toks) (attempt (n+1) (drop 1 toks))

  isMod ((ty, (_,x)) : xs) = case ty of
                               Conid  -> Just (toImp x,xs)
                               Qconid -> Just (toImp x,xs)
                               _      -> Nothing
  isMod _                   = Nothing

  toImp x = Import { impMod = splitModName x, impType = isSrc }
  isSrc   = case ts of
              _ : (_,(_,x)) : _ | isSourcePragma x -> SourceImp
              _                                    -> NormalImp



parse              :: [PosToken] -> (ModName,[Import])
parse ((Reservedid,(_,"module")) : (_,(_,m)) : is) =
                                                  (splitModName m,imports is)
parse is            = (([],"Main"),imports is)

imports            :: [PosToken] -> [Import]
imports ts          = case isImp $ snd $ break (("import" ==) . snd . snd) ts of
                        Just (x,xs) -> x : imports xs
                        _           -> []

-- | A hierarchical module name.
type Qualifier      = [String]
type ModName        = (Qualifier,String)


-- | Convert a string name into a hierarchical name qualifier.
splitQualifier     :: String -> Qualifier
splitQualifier cs   = case break ('.'==) cs of
                        (xs,_:ys)  -> xs : splitQualifier ys
                        _          -> [cs]

-- | Convert a string name into a hierarchical name.
splitModName       :: String -> ModName
splitModName cs     = case break ('.'==) cs of
                        (xs,_:ys)  -> let (as,bs) = splitModName ys
                                   in (xs:as,bs)
                        _ -> ([],cs)

joinModName        :: ModName -> String
joinModName (xs,y)  = concat $ intersperse "." (xs ++ [y])

-- | The files in which a module might reside.
relPaths           :: ModName -> [FilePath]
relPaths (xs,y)     = [ prefix ++ suffix | suffix <- suffixes ]
  where prefix      = foldr (</>) y xs

suffixes           :: [String]
suffixes            = [".hs",".lhs", ".imports"]

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



