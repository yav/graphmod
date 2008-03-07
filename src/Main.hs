import Utils

import Control.Monad(when)
import Control.Monad.Fix(mfix)
import Data.List(intersperse,elemIndex)
import Data.Maybe(mapMaybe,isJust)
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import System.FilePath
import System.Directory
import System.Console.GetOpt


main = do xs <- getArgs
          let (fs, ms, errs) = getOpt Permute options xs
          case errs of
            [] -> do let opts = foldr ($) default_opts fs
                     g <- graph (add_current opts) (map to_input ms)
                     putStrLn (make_dot g)
            _ -> hPutStrLn stderr $ usageInfo "mods" options


-- Guess if we have a file or a module name
to_input m
  | e `elem` suffixes = File m
  | otherwise         = Module (splitModName m)
  where (f,e) = splitExtension m




type Edges  = [(Int,[Int])]
data Input  = File FilePath | Module ModName

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


-- Render edges and a triw into the dot language
-- XXX: Use Andy's lib
--------------------------------------------------------------------------------
make_dot (es,trie) =
  "digraph {\n" ++ fst (to_dot 0 colors trie)
                ++ unlines (map edges es) ++ "}\n"
  where edges (x,ys) = show x ++ "-> {" ++ unwords (map show ys) ++ "}"


to_dot s cs (Sub ts (n:ns))  = let (txt,s1) = to_dot s cs (Sub ts ns)
                               in (node n ++ txt,s1)
  where node (l,x) = show x ++ "[label=\"" ++ l ++ "\"]\n"
to_dot s (c:cs) (Sub ((a,t):ts) [])  =
  let (txt1,s1) = to_dot (s+1) cs t
      (txt2,s2) = to_dot s1 (c:cs) (Sub ts [])
  in (cluster ++ txt1 ++ "}\n" ++ txt2,s2)
  where cluster = unlines [ "subgraph cluster_" ++ show s ++ "{"
                          , "label = \"" ++ a ++ "\""
                          , "labeljust = \"r\""
                          , "color=\"" ++ c ++ "\""
                          , "style = \"filled\""
                          ]
to_dot s _ _ = ("\n",s)



colors = ["#ccffcc", "#99ff99", "#66ff66", "#669966" ] ++
         ["#ffcccc", "#ff9999", "#ff6666", "#996666" ] ++
         ["#ccccff", "#9999ff", "#6666ff", "#666699" ] ++
         repeat "#cccccc"


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
  }

default_opts = Opts
  { inc_dirs      = []
  , quiet         = False
  , with_missing  = False
  }

add_current o = case inc_dirs o of
                  [] -> o { inc_dirs = ["."] }
                  _  -> o
options =
  [ Option ['q'] ["quiet"] (NoArg set_quiet) "Do not show warnings"
  , Option ['i'] []        (ReqArg add_inc "DIR") "Add a search directory"
  , Option ['a'] ["all"]   (NoArg set_all)   "Add nodes for missing modules"
  ]

set_quiet o = o { quiet = True }
set_all o   = o { with_missing = True }
add_inc d o = o { inc_dirs = d : inc_dirs o }
