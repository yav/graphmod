import Utils
import qualified Trie

import Text.Dot

import Control.Monad(when,forM_,(<=<))
import Control.Monad.Fix(mfix)
import Data.List(intersperse)
import Data.Maybe(mapMaybe,isJust,fromMaybe)
import qualified Data.IntMap as Map
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import System.FilePath
import System.Console.GetOpt
import Numeric(showHex)

main :: IO ()
main = do xs <- getArgs
          let (fs, ms, errs) = getOpt Permute options xs
          case errs of
            [] -> do let opts = foldr ($) default_opts fs
                     g <- graph (add_current opts) (map to_input ms)
                     putStrLn (make_dot (use_clusters opts) g)
            _ -> hPutStrLn stderr $ usageInfo "mods" options


data Input  = File FilePath | Module ModName

-- | Guess if we have a file or a module name
to_input :: String -> Input
to_input m
  | takeExtension m `elem` suffixes = File m
  | otherwise                       = Module (splitModName m)



type Nodes    = Trie.Trie String [(String,Int)]
type Edges    = Map.IntMap [Int]

graph :: Opts -> [Input] -> IO (Edges, Nodes)
graph opts inputs = mfix $ \ ~(_,mods) ->
  -- NOTE: 'mods' is the final value of 'done' in the funciton 'loop'.

  let loop :: Nodes -> Edges -> Int -> [Input] -> IO (Edges, Nodes)

      loop done es _ [] = return (es,done)

      loop done es size (Module m : todo)
        | ignore done m = loop done es size todo
        | otherwise =
          do fs <- modToFile (inc_dirs opts) m
             case fs of
               []     -> do warn opts (notFoundMsg m)
                            if with_missing opts
                              then add done es size m [] todo
                              else loop done es size todo
               f : gs -> do when (not (null gs)) (warn opts (ambigMsg m fs))
                            (x,imps) <- parseFile f
                            add done es size x imps todo
    
      loop done es size (File f : todo) =
        do (m,is) <- parseFile f
           if ignore done m
             then loop done es size todo
             else add done es size m is todo

      add done es size m imps ms =
        let ms1     = map Module imps ++ ms
            imp_ids = mapMaybe nodeFor imps
            size1   = 1 + size
        in size1 `seq`
            loop (insMod m size done) (Map.insert size imp_ids es) size1 ms1

      nodeFor x         = lookupMod x mods
      insMod (q,m) n t  = Trie.insert q (\xs -> (m,n) : fromMaybe [] xs) t
      lookupMod (q,m)   = lookup m <=< Trie.lookup q
      ignore done m     = elem m (ignore_mods opts) || isJust (lookupMod m done)

  in loop Trie.empty Map.empty 0 inputs
  

-- We use tries to group modules by directory.
--------------------------------------------------------------------------------



-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------
make_dot :: Bool -> (Edges,Nodes) -> String
make_dot cl (es,t) =
  showDot $
  do if cl then make_clustered_dot 0 t
           else make_unclustered_dot 0 "" t >> return ()
     forM_ (Map.toList es) $ \(x,ys) ->
                        forM_ ys $ \y -> userNodeId x .->. userNodeId y


make_clustered_dot :: Int -> Nodes -> Dot ()
make_clustered_dot c (Trie.Sub xs ys) =
  do forM_ (fromMaybe [] ys) $ \(ls,n) -> userNode (userNodeId n) [("label",ls)]
     forM_ xs $ \(name,sub) ->
       cluster $
       do attribute ("label", name)
          attribute ("color" , colors !! c)
          attribute ("style", "filled")
          let c1 = c + 1
          c1 `seq` make_clustered_dot c1 sub


make_unclustered_dot :: Int -> String -> Nodes -> Dot Int
make_unclustered_dot c pre (Trie.Sub xs ys') =
  do let ys = fromMaybe [] ys'
     forM_ ys $ \(ls,n) -> userNode (userNodeId n) [ ("label", pre ++ ls)
                                                   , ("color", colors !! c)
                                                   , ("style", "filled")
                                                   ]
     let c1 = if null ys then c else c + 1
     c1 `seq` loop xs c1
  where
  loop ((name,sub):ms) c1 =
    do let pre1 = pre ++ name ++ "."
       c2 <- make_unclustered_dot c1 pre1 sub
       loop ms c2
  loop [] c2 = return c2


type Color = (Int,Int,Int)

-- XXX: generate all?
colors :: [String]
colors = map col (ys1 ++ ys2) ++ repeat "#cccccc"
  where
  xs1  :: [Color]
  xs1   = [ (1,0,1), (2,0,2), (3,0,3), (2,1,2), (3,1,3), (3,2,3) ]
  xs2   = map rotR xs1
  xs3   = map rotR xs2
  ys1   = xs1 ++ xs2 ++ xs3
  ys2   = map compl (reverse ys1)

  col (x,y,z)   = '#' : (showHex (mk x) $ showHex (mk y) $ showHex (mk z) "")
  mk n          = 0xFF - n * 0x33
  rotR (x,y,z)  = (z,x,y)
  compl (x,y,z) = (3-x,3-y,3-z)

-- Warnings and error messages
--------------------------------------------------------------------------------
warn               :: Opts -> String -> IO ()
warn o _ | quiet o  = return ()
warn _ msg          = hPutStrLn stderr ("WARNING: " ++ msg)

notFoundMsg        :: ModName -> String
notFoundMsg m       = "Cannot find a file for module "
                                      ++ joinModName m ++ " (ignoring)"

ambigMsg           :: ModName -> [FilePath] -> String
ambigMsg m xs       = "Multiple files for module " ++ joinModName m
                   ++ " (picking the first):\n"
                   ++ concat (intersperse "," xs)

-- Command line options
--------------------------------------------------------------------------------
data Opts = Opts
  { inc_dirs      :: [FilePath]
  , quiet         :: Bool
  , with_missing  :: Bool
  , use_clusters  :: Bool
  , ignore_mods   :: [ModName]
  }

type OptT = Opts -> Opts

default_opts :: Opts
default_opts = Opts
  { inc_dirs      = []
  , quiet         = False
  , with_missing  = False
  , use_clusters  = True
  , ignore_mods   = []
  }

options :: [OptDescr OptT]
options =
  [ Option ['q'] ["quiet"] (NoArg set_quiet) "Do not show warnings"
  , Option ['i'] []        (ReqArg add_inc "DIR") "Add a search directory"
  , Option ['a'] ["all"]   (NoArg set_all)   "Add nodes for missing modules"
  , Option []    ["no-cluster"] (NoArg set_no_cluster)
                                             "Do not cluster directories"
  , Option ['r'] ["remove-module"] (ReqArg add_ignore_mod "MODULE")
                                            "Remove a module from the graph"

  ]

add_current      :: OptT
add_current o     = case inc_dirs o of
                      [] -> o { inc_dirs = ["."] }
                      _  -> o

set_quiet        :: OptT
set_quiet o       = o { quiet = True }

set_all          :: OptT
set_all o         = o { with_missing = True }

set_no_cluster   :: OptT
set_no_cluster o  = o { use_clusters = False }

add_inc          :: FilePath -> OptT
add_inc d o       = o { inc_dirs = d : inc_dirs o }

add_ignore_mod :: String -> OptT
add_ignore_mod m o = o { ignore_mods = splitModName m : ignore_mods o }
