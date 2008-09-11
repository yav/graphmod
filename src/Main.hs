import Utils

import Text.Dot

import Control.Monad(when,forM_)
import Control.Monad.Fix(mfix)
import Data.List(intersperse)
import Data.Maybe(mapMaybe,isJust)
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import System.FilePath
import System.Console.GetOpt
import Numeric(showHex)

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




type Edges  = [(Int,[Int])]

graph :: Opts -> [Input] -> IO (Edges, Trie)
graph opts ms = mfix (\ ~(_,g) -> loop g empty [] 0 ms)
  where
  loop :: Trie -> Trie -> Edges -> Int -> [Input] -> IO (Edges, Trie)
  loop _ done es _ [] = return (es,done)
  loop mods done es size (Module m:ms)
    | isJust (lkp done m) = loop mods done es size ms
    | otherwise =
      do fs <- modToFile (inc_dirs opts) m
         let size1 = size + 1
         seq size1 $ case fs of
           []     -> do warn opts (notFoundMsg m)
                        if with_missing opts
                          then add mods done es size m [] ms
                          else loop mods done es size ms
           f : gs -> do when (not (null gs)) (warn opts (ambigMsg m fs))
                        (x,imps) <- parseFile f
                        add mods done es size x imps ms

  loop mods done es size (File f:ms) =
    do (m,is) <- parseFile f
       case lkp done m of
         Just {} -> loop mods done es size ms
         Nothing -> add mods done es size m is ms

  add mods done es size m imps ms =
    let deps = mapMaybe (lkp mods) imps
        size1 = size + 1
    in size1 `seq` loop mods (ins (m,size) done)
                                  ((size,deps) : es)
                                  size1
                                  (map Module imps ++ ms)



-- We use tries to group modules by directory.
--------------------------------------------------------------------------------

-- | The labels on the nodes correspond to directories,
-- the nodes on the leaves correspond to (unqualified) modules.
-- Eeach module has a unique number.
data Trie = Sub [(String, Trie)] [(String,Int)] deriving Show
empty = Sub [] []

lkp (Sub _ bs) ([],a)   = lookup a bs
lkp (Sub as _) (k:ks,a) = (`lkp` (ks,a)) =<< lookup k as


ins (([],x),n) (Sub as bs)    = Sub as ((x,n):bs)
ins ((a:as,x),n) (Sub ts bs)  = Sub (upd ts) bs
  where new = ((as,x),n)

        upd ((k,t):ts)
          | k < a     = (k,t) : upd ts
          | k == a    = (k, ins new t) : ts
        upd ts = (a, ins new empty) : ts


-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------

make_dot cl (es,t) =
  showDot $
  do if cl then make_clustered_dot 0 t
           else make_unclustered_dot 0 "" t >> return ()
     forM_ es $ \(x,ys) -> forM_ ys $ \y -> userNodeId x .->. userNodeId y


make_clustered_dot c (Sub xs ys) =
  do forM_ ys $ \(xs,n) -> userNode (userNodeId n) [("label",xs)]
     forM_ xs $ \(name,sub) ->
       cluster $
       do attribute ("label", name)
          attribute ("color" , colors !! c)
          attribute ("style", "filled")
          let c1 = c + 1
          c1 `seq` make_clustered_dot c1 sub


make_unclustered_dot c pre (Sub xs ys) =
  do forM_ ys $ \(xs,n) -> userNode (userNodeId n) [ ("label", pre ++ xs)
                                                   , ("color", colors !! c)
                                                   , ("style", "filled")
                                                   ]
     let c1 = if null ys then c else c + 1
     c1 `seq` loop xs c1
  where
  loop ((name,sub):xs) c1 =
    do let pre1 = pre ++ name ++ "."
       c2 <- make_unclustered_dot c1 pre1 sub
       loop xs c2
  loop [] c2 = return c2



-- XXX: generate all?
colors = map col (ys1 ++ ys2) ++ repeat "#cccccc"
  where
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
err msg             = error ("ERROR: " ++ msg)

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
  { inc_dirs :: [FilePath]
  , quiet :: Bool
  , with_missing :: Bool
  , use_clusters :: Bool
  }

default_opts = Opts
  { inc_dirs      = []
  , quiet         = False
  , with_missing  = False
  , use_clusters  = True
  }

add_current o = case inc_dirs o of
                  [] -> o { inc_dirs = ["."] }
                  _  -> o
options =
  [ Option ['q'] ["quiet"] (NoArg set_quiet) "Do not show warnings"
  , Option ['i'] []        (ReqArg add_inc "DIR") "Add a search directory"
  , Option ['a'] ["all"]   (NoArg set_all)   "Add nodes for missing modules"
  , Option []    ["no-cluster"] (NoArg set_no_cluster)
                                             "Do not cluster directories"
  ]

set_quiet o = o { quiet = True }
set_all o   = o { with_missing = True }
add_inc d o = o { inc_dirs = d : inc_dirs o }
set_no_cluster o = o { use_clusters = False }


