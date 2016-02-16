import Utils
import qualified Trie
import CabalSupport(parseCabalFile,Unit(..))

import Text.Dot

import Control.Monad(when,forM_,(<=<),mplus,msum,guard)
import Control.Monad.Fix(mfix)
import Control.Exception(catch,SomeException(..))
import Data.List(intersperse,partition)
import Data.Maybe(mapMaybe,isJust,fromMaybe,listToMaybe)
import qualified Data.IntMap as IMap
import qualified Data.Map    as Map
import qualified Data.IntSet as Set
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import System.FilePath
import System.Console.GetOpt
import System.Directory(getDirectoryContents)
import Numeric(showHex)

import Paths_graphmod (version)
import Data.Version (showVersion)

main :: IO ()
main = do xs <- getArgs
          let (fs, ms, errs) = getOpt Permute options xs
          case errs of
            [] | show_version opts ->
                  putStrLn ("graphmod " ++ showVersion version)

               | otherwise ->
                  do (incs,inps) <- fromCabal (use_cabal opts)
                     g <- graph (foldr add_inc (add_current opts) incs)
                                (inps ++ map to_input ms)
                     putStr (make_dot (graph_size opts) (color_scheme opts)
                                                      (use_clusters opts) g)
              where opts = foldr ($) default_opts fs

            _ -> hPutStrLn stderr $
                  usageInfo "usage: graphmod MODULES/PATHS" options


data Input  = File FilePath | Module ModName
              deriving Show

-- | Guess if we have a file or a module name
to_input :: String -> Input
to_input m
  | takeExtension m `elem` suffixes = File m
  | otherwise                       = Module (splitModName m)



-- type Nodes    = Trie.Trie String [(String,Int)]
type NodesC   = Trie.Trie String [((NodeT,String),Int)]
                    -- Maps a path to:   ((node, label), nodeId)

type Edges    = IMap.IntMap Set.IntSet


data NodeT    = ModuleNode
              | CollapsedNode Bool NodesC
                -- ^ indicates if it contains module too.
                -- Also includes the collapsed tree, so we know whether to
                -- draw edges "into" the collapsed node or not.
                deriving (Show,Eq,Ord)

graph :: Opts -> [Input] -> IO (Edges, NodesC)
graph opts inputs = fmap maybePrune $ mfix $ \ ~(_,mods) ->
  -- NOTE: 'mods' is the final value of 'done' in the funciton 'loop'.

  let nodeFor x         = lookupNode x mods  -- Recursion happens here!

      loop :: NodesC -> Edges -> Int -> [Input] -> IO (Edges, NodesC)

      loop done es _ [] = return (es, collapseAll done (collapse_quals opts))

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
                        Just n  -> IMap.insertWith Set.union n imp_ids es
                        Nothing -> es
        in size1 `seq` loop (insMod m size done) es1 size1 ms1

      insMod (q,m) n t  = Trie.insert q (\xs -> ((ModuleNode,m),n)
                                                          : fromMaybe [] xs) t
      lookupMod (q,m)   = lookup (ModuleNode,m) <=< Trie.lookup q
      ignore done m     = isIgnored (ignore_mods opts) m
                       || isJust (lookupMod m done)

  in loop Trie.empty IMap.empty 0 inputs

  where
  maybePrune (es,ns) = if prune_edges opts then (pruneEdges es, ns)
                                           else (es,ns)




pruneEdges :: Edges -> Edges
pruneEdges es = foldr checkEdges es (IMap.toList es)
  where
  reachIn _ _ _ [] = False
  reachIn g tgt visited (x : xs)
    | x `Set.member` visited  = reachIn g tgt visited xs
    | x == tgt                = True
    | otherwise = let vs = neighbours g x
                  in reachIn g tgt (Set.insert x visited) (vs ++ xs)

  neighbours g x = Set.toList (IMap.findWithDefault Set.empty x g)

  reachableIn g x y = reachIn g y Set.empty [x]

  rmEdge x y g = IMap.adjust (Set.delete y) x g

  checkEdge x y g = let g1 = rmEdge x y g
                    in if reachableIn g1 x y then g1 else g

  checkEdges (x,vs) g = foldr (checkEdge x) g (Set.toList vs)


isIgnored :: IgnoreSet -> ModName -> Bool
isIgnored (Trie.Sub _ (Just IgnoreAll))       _        = True
isIgnored (Trie.Sub _ (Just (IgnoreSome ms))) ([],m)   = elem m ms
isIgnored (Trie.Sub _ Nothing)                ([],_)   = False
isIgnored (Trie.Sub ts _)                     (q:qs,m) =
  case Map.lookup q ts of
    Nothing -> False
    Just t  -> isIgnored t (qs,m)

lookupNode :: ModName -> NodesC -> Maybe Int
lookupNode ([],m) (Trie.Sub _ mb) = lookupBy (containsModule m) =<< mb

lookupNode (q:qs,m) (Trie.Sub ts mb) =
  (lookupBy containsNode =<< mb) `mplus`
  (lookupNode (qs,m) =<< Map.lookup q ts)

  where
  containsNode :: (NodeT, String) -> Bool
  containsNode (CollapsedNode _ t, q1) = q == q1 && isJust (lookupNode (qs,m) t)
  containsNode (ModuleNode, _)         = False

containsModule :: String -> (NodeT, String) -> Bool
containsModule q (ModuleNode, q1)              = q == q1
containsModule q (CollapsedNode withMod _, q1) = withMod && q == q1

lookupBy :: (a -> Bool) -> [(a,b)] -> Maybe b
lookupBy p xs = listToMaybe [ y | (x,y) <- xs, p x ]




-- XXX: We could combine collapseAll and collapse into a single pass
-- to avoid traversing form the root each time.
collapseAll :: NodesC -> Trie.Trie String Bool -> NodesC
collapseAll t0 = foldr (\q t -> fromMaybe t (collapse t q)) t0 . toList
  where
  toList (Trie.Sub _ (Just x))  = return ([], x)
  toList (Trie.Sub as Nothing)  = do (q,t)  <- Map.toList as
                                     (qs,x) <- toList t
                                     return (q:qs, x)

-- NOTE: We use the Maybe type to indicate when things changed.
collapse :: NodesC -> (Qualifier,Bool) -> Maybe NodesC
collapse _ ([],_) = return Trie.empty      -- Probably not terribly useful.

collapse (Trie.Sub ts mb) ([q],alsoMod) =
  do (n,withMod) <- fmap (\x -> (x,True)) useMod
            `mplus` fmap (\x -> (x,False)) (getFirst =<< thisTrie)

     return $ Trie.Sub (Map.delete q ts)
            $ Just $ ((CollapsedNode withMod collapsedTrie, q),n)
                   : if withMod then others else allNodes

  where thisTrie          = Map.lookup q ts
        collapsedTrie     = fromMaybe Trie.empty thisTrie
        allNodes          = fromMaybe [] mb
        (thisNode,others) = partition (containsModule q . fst) allNodes
        useMod            = do guard alsoMod
                               listToMaybe (map snd thisNode)

        getFirst (Trie.Sub ts1 ms) =
          msum (fmap snd (listToMaybe =<< ms) : map getFirst (Map.elems ts1))

collapse (Trie.Sub ts ms) (q : qs,x) =
  do t <- Map.lookup q ts
     t1 <- collapse t (qs,x)
     return (Trie.Sub (Map.insert q t1 ts) ms)




-- We use tries to group modules by directory.
--------------------------------------------------------------------------------



-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------
make_dot :: String -> Int -> Bool -> (Edges,NodesC) -> String
make_dot sz col cl (es,t) =
  showDot $
  do attribute ("size", sz)
     attribute ("ratio", "fill")
     if cl then make_clustered_dot (colors col) t
           else make_unclustered_dot (colors col) "" t >> return ()
     forM_ (IMap.toList es) $ \(x,ys) ->
       forM_ (Set.toList ys) $ \y -> userNodeId x .->. userNodeId y



make_clustered_dot :: [Color] -> NodesC -> Dot ()
make_clustered_dot c (Trie.Sub xs ys) =
  do let col = renderColor (head c)
     forM_ (fromMaybe [] ys) $ \((t,ls),n) ->
       userNode (userNodeId n) $
       [ ("label",ls) ] ++
       case t of
         CollapsedNode False _ -> [ ("shape", "box")
                                  , ("style","filled")
                                  , ("color", col)
                                  ]
         CollapsedNode True  _ -> [ ("shape", "box")
                                  , ("fillcolor", col)
                                  , ("style","filled")
                                  ]
         ModuleNode            -> []

     forM_ (Map.toList xs) $ \(name,sub) ->
       cluster $
       do attribute ("label", name)
          attribute ("color" , col)
          attribute ("style", "filled")
          make_clustered_dot (tail c) sub


make_unclustered_dot :: [Color] -> String -> NodesC -> Dot [Color]
make_unclustered_dot c pre (Trie.Sub xs ys') =
  do let col = renderColor (head c)
     let ys = fromMaybe [] ys'
     forM_ ys $ \((t,ls),n) ->
        userNode (userNodeId n) $
              [ ("fillcolor", col)
              , ("style", "filled")
              , ("label", pre ++ ls)
              ] ++
            case t of
              CollapsedNode False _ -> [ ("shape", "box"), ("color", col) ]
              CollapsedNode True  _ -> [ ("shape", "box") ]
              ModuleNode            -> []

     let c1 = if null ys then c else tail c
     c1 `seq` loop (Map.toList xs) c1
  where
  loop ((name,sub):ms) c1 =
    do let pre1 = pre ++ name ++ "."
       c2 <- make_unclustered_dot c1 pre1 sub
       loop ms c2
  loop [] c2 = return c2


type Color = (Int,Int,Int)

colors :: Int -> [Color]
colors n = light_dark $ drop n $ cycle colorses

renderColor :: Color -> String
renderColor (x,y,z) = '#' : showHex (mk x) (showHex (mk y) (showHex (mk z) ""))
  where mk n = 0xFF - n * 0x33


light_dark :: [[a]] -> [a]
light_dark (xs : ys : zs) = xs ++ reverse ys ++ light_dark zs
light_dark [x]            = x
light_dark []             = []



colorses :: [[Color]]
colorses = [green, cyan, blue, magenta, red, yellow]
  where
  red :: [Color]
  red   = [ (0,1,1), (0,2,2), (0,3,3), (1,2,2), (1,3,3), (2,3,3) ]
  green = map rotR red
  blue  = map rotR green
  [cyan,magenta,yellow] = map (map compl . reverse) [red, green, blue]

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


--------------------------------------------------------------------------------


fromCabal :: Bool -> IO ([FilePath],[Input])
fromCabal True =
  do fs <- getDirectoryContents "." -- XXX
     case filter ((".cabal" ==) . takeExtension) fs of
       f : _ -> do units <- parseCabalFile f
                              `catch` \SomeException {} -> return []
                   return (fromUnits units)
       _ -> return ([],[])
fromCabal _ = return ([],[])


fromUnits :: [Unit] -> ([FilePath], [Input])
fromUnits us = (concat fs, concat is)
  where
  (fs,is) = unzip (map fromUnit us)

fromUnit :: Unit -> ([FilePath], [Input])
fromUnit u = (unitPaths u, map File (unitFiles u) ++ map Module (unitModules u))



-- Command line options
--------------------------------------------------------------------------------
data Opts = Opts
  { inc_dirs      :: [FilePath]
  , quiet         :: Bool
  , with_missing  :: Bool
  , use_clusters  :: Bool
  , ignore_mods   :: IgnoreSet
  , collapse_quals :: Trie.Trie String Bool
    -- ^ The "Bool" tells us if we should collapse modules as well.
    -- For example, "True" says that A.B.C would collapse not only A.B.C.*
    -- but also the module A.B.C, if it exists.
  , show_version  :: Bool
  , color_scheme  :: Int
  , prune_edges   :: Bool
  , graph_size    :: String

  , use_cabal     :: Bool -- ^ should we try to use a cabal file, if any
  }

type IgnoreSet  = Trie.Trie String IgnoreSpec
data IgnoreSpec = IgnoreAll | IgnoreSome [String]  deriving Show

type OptT = Opts -> Opts

default_opts :: Opts
default_opts = Opts
  { inc_dirs        = []
  , quiet           = False
  , with_missing    = False
  , use_clusters    = True
  , ignore_mods     = Trie.empty
  , collapse_quals  = Trie.empty
  , show_version    = False
  , color_scheme    = 0
  , prune_edges     = False
  , graph_size      = "6,4"
  , use_cabal       = True
  }

options :: [OptDescr OptT]
options =
  [ Option ['q'] ["quiet"] (NoArg set_quiet)
    "Do not show warnings"

  , Option ['i'] []        (ReqArg add_inc "DIR")
    "Add a search directory"

  , Option ['a'] ["all"]   (NoArg set_all)
    "Add nodes for missing modules"

  , Option []    ["no-cluster"] (NoArg set_no_cluster)
    "Do not cluster directories"

  , Option ['r'] ["remove-module"] (ReqArg add_ignore_mod "NAME")
    "Do not display module NAME"

  , Option ['R'] ["remove-qual"]   (ReqArg add_ignore_qual "NAME")
    "Do not display modules NAME.*"

  , Option ['c'] ["collapse"]   (ReqArg (add_collapse_qual False) "NAME")
    "Display modules NAME.* as one node"

  , Option ['C'] ["collapse-module"] (ReqArg (add_collapse_qual True) "NAME")
    "Display modules NAME and NAME.* as one node"

  , Option ['p'] ["prune-edges"] (NoArg set_prune)
    "Remove imports if the module is imported by another imported module"

  , Option ['d'] ["graph-dim"] (ReqArg set_size "SIZE,SIZE")
    "Set dimensions of the graph.  See the `size` attribute of graphvize."

  , Option ['s'] ["colors"] (ReqArg add_color_scheme "NUM")
    "Choose a color scheme number (0-5)"

  , Option [] ["no-cabal"] (NoArg (set_cabal False))
    "Do not use Cabal for paths and modules."

  , Option ['v'] ["version"]   (NoArg set_show_version)
    "Show the current version."
  ]

add_current      :: OptT
add_current o     = case inc_dirs o of
                      [] -> o { inc_dirs = ["."] }
                      _  -> o

set_quiet        :: OptT
set_quiet o       = o { quiet = True }

set_show_version :: OptT
set_show_version o = o { show_version = True }

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

add_color_scheme :: String -> OptT
add_color_scheme n o = o { color_scheme = case reads n of
                                            [(x,"")] -> x
                                            _ -> color_scheme default_opts }

add_collapse_qual :: Bool -> String -> OptT
add_collapse_qual m s o = o { collapse_quals = upd (splitQualifier s)
                                                      (collapse_quals o) }

  where
  upd [] (Trie.Sub xs (Just _)) = Trie.Sub xs (Just m)
  upd _ t@(Trie.Sub _ (Just _)) = t
  upd [] _                      = Trie.Sub Map.empty (Just m)
  upd (q:qs) (Trie.Sub as _)    = Trie.Sub (Map.alter add q as) Nothing
    where add j = Just $ upd qs $ fromMaybe Trie.empty j

set_prune :: OptT
set_prune o = o { prune_edges = True }

set_size :: String -> OptT
set_size s o = o { graph_size = s }

set_cabal :: Bool -> OptT
set_cabal on o = o { use_cabal = on }

