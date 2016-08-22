import Utils
import qualified Trie
import CabalSupport(parseCabalFile,Unit(..))

import Text.Dot

import Control.Monad(when,forM_,mplus,msum,guard)
import Control.Monad.Fix(mfix)
import Control.Exception(catch,SomeException(..))
import Data.List(intersperse,partition,transpose)
import Data.Maybe(isJust,fromMaybe,listToMaybe)
import qualified Data.IntMap as IMap
import qualified Data.Map    as Map
import qualified Data.IntSet as ISet
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
                     putStr (make_dot opts g)
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



type NodesC   = Trie.Trie String [((NodeT,String),Int)]
                    -- Maps a path to:   ((node, label), nodeId)

type Edges    = IMap.IntMap ISet.IntSet

data NodeT    = ModuleNode
              | ModuleInItsCluster
              | CollapsedNode Bool NodesC
                -- ^ indicates if it contains module too.
                -- Also includes the collapsed tree, so we know whether to
                -- draw edges "into" the collapsed node or not.
                deriving (Show,Eq,Ord)

data AllEdges = AllEdges
  { normalEdges   :: Edges
  , sourceEdges   :: Edges
  }

noEdges :: AllEdges
noEdges = AllEdges { normalEdges    = IMap.empty
                   , sourceEdges    = IMap.empty
                   }

graph :: Opts -> [Input] -> IO (AllEdges, NodesC)
graph opts inputs = fmap maybePrune $ mfix $ \ ~(_,mods) ->
  -- NOTE: 'mods' is the final value of 'done' in the funciton 'loop'.

  let nodeFor x         = lookupMod x mods    -- Recursion happens here!

      loop :: NodesC ->
              AllEdges    {- all kinds of edges -} ->
              Int         {- size -} ->
              [Input]     {- root files/modules -} ->
              IO (AllEdges, NodesC)

      loop done es _ [] =
        return (es, collapseAll done (collapse_quals opts))

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

      add done es size m imps ms = size1 `seq` loop done1 es1 size1 ms1
        where
        es1   = case nodeFor m of
                  Just src -> foldr (addEdge src) es imps
                  Nothing  -> es
        size1 = size + 1
        ms1   = map (Module . impMod) imps ++ ms
        done1 = insMod m size done


      addEdge nFrom i aes =
        case nodeFor (impMod i) of
          Nothing  -> aes
          Just nTo ->
            case impType i of
              SourceImp ->
                aes { sourceEdges = insSet nFrom nTo (sourceEdges aes) }
              NormalImp ->
                aes { normalEdges = insSet nFrom nTo (normalEdges aes) }


  in loop Trie.empty noEdges 0 inputs

  where
  maybePrune (es,ns)
    | prune_edges opts  = (es { normalEdges = pruneEdges (normalEdges es) }, ns)
    | otherwise         = (es,ns)

  ignore done m         = isIgnored (ignore_mods opts) m
                       || isJust (lookupMod m done)




lookupMod :: ModName -> NodesC -> Maybe Int
lookupMod (q,m) t = lookup (ModuleNode,m) =<< Trie.lookup q t

insMod :: ModName -> Int -> NodesC -> NodesC
insMod (q,m) n t  = Trie.insert q ins t
  where
  ins xs = case xs of
             Nothing -> [ ((ModuleNode,m),n) ]
             Just ys -> ((ModuleNode,m),n) : ys

insSet :: Int -> Int -> Edges -> Edges
insSet x y m = IMap.insertWith ISet.union x (ISet.singleton y) m



pruneEdges :: Edges -> Edges
pruneEdges es = foldr checkEdges es (IMap.toList es)
  where
  reachIn _ _ _ [] = False
  reachIn g tgt visited (x : xs)
    | x `ISet.member` visited = reachIn g tgt visited xs
    | x == tgt                = True
    | otherwise = let vs = neighbours g x
                  in reachIn g tgt (ISet.insert x visited) (vs ++ xs)

  neighbours g x = ISet.toList (IMap.findWithDefault ISet.empty x g)

  reachableIn g x y = reachIn g y ISet.empty [x]

  rmEdge x y g = IMap.adjust (ISet.delete y) x g

  checkEdge x y g = let g1 = rmEdge x y g
                    in if reachableIn g1 x y then g1 else g

  checkEdges (x,vs) g = foldr (checkEdge x) g (ISet.toList vs)


isIgnored :: IgnoreSet -> ModName -> Bool
isIgnored (Trie.Sub _ (Just IgnoreAll))       _        = True
isIgnored (Trie.Sub _ (Just (IgnoreSome ms))) ([],m)   = elem m ms
isIgnored (Trie.Sub _ Nothing)                ([],_)   = False
isIgnored (Trie.Sub ts _)                     (q:qs,m) =
  case Map.lookup q ts of
    Nothing -> False
    Just t  -> isIgnored t (qs,m)

containsModule :: String -> (NodeT, String) -> Bool
containsModule q (ModuleNode, q1)             = q == q1
containsModule _ (ModuleInItsCluster, _) =
  error "[bug] ModuleInItsCluster while collapsing"
containsModule q (CollapsedNode withMod _, q1) = withMod && q == q1



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



-- | If inside cluster A.B we have a module M,
-- and there is a cluster A.B.M, then move M into that cluster as a special node
moveModulesInCluster :: NodesC -> NodesC
moveModulesInCluster (Trie.Sub su0 ms) = goMb (fmap moveModulesInCluster su0) ms
  where
  goMb su mb =
    case mb of
      Nothing -> Trie.Sub su Nothing
      Just xs -> go [] su xs

  go ns su xs =
    case xs of
      [] -> Trie.Sub su $ case ns of
                            [] -> Nothing
                            _  -> Just ns
      y : ys ->
        case check y su of
          Left it   -> go (it : ns) su ys
          Right su1 -> go ns su1 ys

  check it@((nt,s),i) mps =
    case nt of
      ModuleNode ->
        case Map.lookup s mps of
          Nothing -> Left it
          Just t  -> Right $ Map.insert s (Trie.insert [] add t) mps
            where
            newM   = ((ModuleInItsCluster,s),i)
            add xs = [newM] ++ fromMaybe [] xs


      ModuleInItsCluster -> Left it
      CollapsedNode _ _  -> Left it


-- We use tries to group modules by directory.
--------------------------------------------------------------------------------



-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------
make_dot :: Opts -> (AllEdges,NodesC) -> String
make_dot opts (es,t) =
  showDot $
  do attribute ("size", graph_size opts)
     attribute ("ratio", "fill")
     let cols = colors (color_scheme opts)
     if use_clusters opts
        then make_clustered_dot cols $
               if mod_in_cluster opts then moveModulesInCluster t else t
        else make_unclustered_dot cols "" t >> return ()
     genEdges normalAttr (normalEdges es)
     genEdges sourceAttr (sourceEdges es)
  where
  normalAttr _x _y  = []
  sourceAttr _x _y  = [("style","dashed")]

  genEdges attr edges =
    forM_ (IMap.toList edges) $ \(x,ys) ->
      forM_ (ISet.toList ys) $ \y ->
        edge (userNodeId x) (userNodeId y) (attr x y)






make_clustered_dot :: [Color] -> NodesC -> Dot ()
make_clustered_dot cs su = go (0,0,0) cs su >> return ()
  where
  clusterC = "#0000000F"

  go outer_col ~(this_col:more) (Trie.Sub xs ys) =
    do let outerC = renderColor outer_col
           thisC  = renderColor this_col

       forM_ (fromMaybe [] ys) $ \((t,ls),n) ->
         userNode (userNodeId n) $
         [ ("label",ls) ] ++
         case t of
           CollapsedNode False _ -> [ ("shape", "box")
                                    , ("style","filled")
                                    , ("color", clusterC)
                                    ]
           CollapsedNode True  _ -> [ ("shape", "box")
                                    , ("style","filled")
                                    , ("fillcolor", clusterC)
                                    ]
           ModuleInItsCluster    -> [ ("style","filled,bold")
                                    , ("fillcolor", outerC)
                                    ]

           ModuleNode            -> [ ("style", "filled")
                                    , ("fillcolor", thisC)
                                    , ("penwidth","0")
                                    ]
       goSub this_col more (Map.toList xs)

  goSub _ cs [] = return cs
  goSub outer_col cs ((name,sub) : more) =
    do (_,cs1) <- cluster $ do attribute ("label", name)
                               attribute ("color" , clusterC)
                               attribute ("style", "filled")
                               go outer_col cs sub

       goSub outer_col cs1 more


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
           ModuleInItsCluster    -> error "[bug]: Cluster in unclustered mode"
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
colors n = cycle $ mix_colors $ drop n $ palettes

renderColor :: Color -> String
renderColor (x,y,z) = '#' : showHex (mk x) (showHex (mk y) (showHex (mk z) ""))
  where mk n = 0xFF - n * 0x44


mix_colors :: [[a]] -> [a]
mix_colors css = mk set1 ++ mk set2
  where
  (set1,set2) = unzip $ map (splitAt 3) css
  mk = concat . transpose


palettes :: [[Color]]
palettes = [green, yellow, blue, red, cyan, magenta ]
  where
  red :: [Color]
  red   = [ (0,1,1), (0,2,2), (0,3,3), (1,2,3), (1,3,3), (2,3,3) ]
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
  , mod_in_cluster:: Bool
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
  , mod_in_cluster  = True
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

  , Option []    ["no-module-in-cluster"] (NoArg set_no_mod_in_cluster)
    "Do not place modules matching a cluster's name inside it."

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

set_no_mod_in_cluster :: OptT
set_no_mod_in_cluster o = o { mod_in_cluster = False }

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

