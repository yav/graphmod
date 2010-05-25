import Utils
import qualified Trie

import Text.Dot

import Control.Monad(when,forM_,(<=<),guard,mplus,msum)
import Control.Monad.Fix(mfix)
import Data.List(intersperse)
import Data.Maybe(mapMaybe,isJust,fromMaybe,listToMaybe)
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
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



-- type Nodes    = Trie.Trie String [(String,Int)]
type NodesC   = Trie.Trie String [((NodeT,String),Int)]
type Edges    = Map.IntMap Set.IntSet

data NodeT    = ModuleNode | CollapsedNode
                deriving (Show,Eq,Ord)

graph :: Opts -> [Input] -> IO (Edges, NodesC)
graph opts inputs = mfix $ \ ~(_,mods) ->
  -- NOTE: 'mods' is the final value of 'done' in the funciton 'loop'.

  let loop :: NodesC -> Edges -> Int -> [Input] -> IO (Edges, NodesC)

      loop done es _ [] = return (es, collapseAll (collapse_quals opts) done)

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
            imp_ids = Set.fromList (mapMaybe nodeFor imps)
            size1   = 1 + size
            es1     = case nodeFor m of
                        Just n  -> Map.insertWith Set.union n imp_ids es
                        Nothing -> es
        in size1 `seq` loop (insMod m size done) es1 size1 ms1

      nodeFor x         = lookupNode x mods  -- Recursion happens here!
      insMod (q,m) n t  = Trie.insert q (\xs -> ((ModuleNode,m),n)
                                                          : fromMaybe [] xs) t
      lookupMod (q,m)   = lookup (ModuleNode,m) <=< Trie.lookup q
      ignore done m     = isIgnored (ignore_mods opts) m
                       || isJust (lookupMod m done)

  in loop Trie.empty Map.empty 0 inputs


isIgnored :: IgnoreSet -> ModName -> Bool
isIgnored (Trie.Sub _ (Just IgnoreAll))       _        = True
isIgnored (Trie.Sub _ (Just (IgnoreSome ms))) ([],m)   = elem m ms
isIgnored (Trie.Sub _ Nothing)                ([],_)   = False
isIgnored (Trie.Sub ts _)                     (q:qs,m) =
  case lookup q ts of
    Nothing -> False
    Just t  -> isIgnored t (qs,m)

findDelete :: (a -> Maybe b) -> [a] -> Maybe ([a],b,[a])
findDelete p xs = search xs []
  where search [] _ = Nothing
        search (x : after) before =
          case p x of
            Just b  -> Just (before, b, after)
            Nothing -> search after (x : before)


lookupNode :: ModName -> NodesC -> Maybe Int
lookupNode ([],m) (Trie.Sub _ mb) = lookup (ModuleNode,m) =<< mb

lookupNode (q:qs,m) (Trie.Sub ts mb) =
  (lookup (CollapsedNode,q) =<< mb) `mplus`
  (lookupNode (qs,m) =<< lookup q ts)


collapseAll :: [Qualifier] -> NodesC -> NodesC
collapseAll qs t0 = foldr (\q t -> fromMaybe t (collapse t q)) t0 qs

-- We use the Maybe type to indicate when things changed.
collapse :: NodesC -> Qualifier -> Maybe NodesC
collapse _ [] = return Trie.empty      -- Probably not terribly useful.

collapse (Trie.Sub ts mb) [q] =
  do let match (q1,t) = guard (q == q1) >> return t
     (before, t, after) <- findDelete match ts
     n <- getFirst t
     return $ Trie.Sub (before ++ after) $ Just $ ((CollapsedNode,q),n) :
                                                              fromMaybe [] mb
  where getFirst (Trie.Sub ts1 ms) =
          msum (fmap snd (listToMaybe =<< ms) : map (getFirst . snd) ts1)

collapse (Trie.Sub ts ms) (q : qs) =
  do let match (q1,t) = guard (q == q1) >> return t
     (before,t,after) <- findDelete match ts
     t1 <- collapse t qs
     return (Trie.Sub ((q,t1) : before ++ after) ms)


-- We use tries to group modules by directory.
--------------------------------------------------------------------------------



-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------
make_dot :: Bool -> (Edges,NodesC) -> String
make_dot cl (es,t) =
  showDot $
  do if cl then make_clustered_dot 0 t
           else make_unclustered_dot 0 "" t >> return ()
     forM_ (Map.toList es) $ \(x,ys) ->
       forM_ (Set.toList ys) $ \y -> userNodeId x .->. userNodeId y

make_clustered_dot :: Int -> NodesC -> Dot ()
make_clustered_dot c (Trie.Sub xs ys) =
  do forM_ (fromMaybe [] ys) $ \((t,ls),n) ->
       userNode (userNodeId n) $
       ("label",ls) :
       case t of
         CollapsedNode -> [ ("shape","box")
                          , ("fillcolor",colors !! c)
                          , ("style","filled")
                          ]
         _             -> []
     forM_ xs $ \(name,sub) ->
       cluster $
       do attribute ("label", name)
          attribute ("color" , colors !! c)
          attribute ("style", "filled")
          let c1 = c + 1
          c1 `seq` make_clustered_dot c1 sub


make_unclustered_dot :: Int -> String -> NodesC -> Dot Int
make_unclustered_dot c pre (Trie.Sub xs ys') =
  do let ys = fromMaybe [] ys'
     forM_ ys $ \((t,ls),n) ->
        userNode (userNodeId n) $
            (if t == CollapsedNode then [("shape","box")] else [])
            ++ [ ("label", pre ++ ls)
              , ("fillcolor", colors !! c)
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
  , ignore_mods   :: IgnoreSet
  , collapse_quals :: [Qualifier]
  }

type IgnoreSet  = Trie.Trie String IgnoreSpec
data IgnoreSpec = IgnoreAll | IgnoreSome [String]

type OptT = Opts -> Opts

default_opts :: Opts
default_opts = Opts
  { inc_dirs      = []
  , quiet         = False
  , with_missing  = False
  , use_clusters  = True
  , ignore_mods   = Trie.empty
  , collapse_quals = []
  }

options :: [OptDescr OptT]
options =
  [ Option ['q'] ["quiet"] (NoArg set_quiet)
    "Do not show warnings."

  , Option ['i'] []        (ReqArg add_inc "DIR")
    "Add a search directory."

  , Option ['a'] ["all"]   (NoArg set_all)
    "Add nodes for missing modules."

  , Option []    ["no-cluster"] (NoArg set_no_cluster)
    "Do not cluster directories."

  , Option ['r'] ["remove-module"] (ReqArg add_ignore_mod "MODULE")
    "Remove a module from the graph."

  , Option ['R'] ["remove-qual"]   (ReqArg add_ignore_qual "QUALIFIER")
    "Remove all modules that start with the given qualifier."

  , Option ['c'] ["collapse"]   (ReqArg add_collapse_qual "QUALIFIER")
    "Display modules matching the qualifier as a single node."
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

add_ignore_mod   :: String -> OptT
add_ignore_mod s o = o { ignore_mods = ins (splitModName s) }
  where
  ins (q,m) = Trie.insert q (upd m) (ignore_mods o)

  upd _ (Just IgnoreAll)        = IgnoreAll
  upd m (Just (IgnoreSome ms))  = IgnoreSome (m:ms)
  upd m Nothing                 = IgnoreSome [m]

add_ignore_qual :: String -> OptT
add_ignore_qual s o = o { ignore_mods = Trie.insert (splitQualifier s)
                                          (const IgnoreAll) (ignore_mods o) }

-- XXX: spot nesting
add_collapse_qual :: String -> OptT
add_collapse_qual s o = o { collapse_quals = splitQualifier s :
                                                          collapse_quals o }



