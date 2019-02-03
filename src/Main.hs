{-# LANGUAGE MultiWayIf #-}
import Utils
import qualified Trie
import CabalSupport(parseCabalFile,Unit(..))
import Text.Dot

import Control.Arrow((***),first)
import Control.Monad(when,forM_,mapM_,msum,guard,unless)
import Control.Monad.Fix(mfix)
import           Control.Exception (SomeException(..))
import qualified Control.Exception as X (catch)
import Data.List(intersperse,intersect,intercalate,transpose,(\\),nub,elemIndex)
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

-- import Paths_graphmod (version)
import Data.Version (showVersion)

main :: IO ()
main = do xs <- getArgs
          let (fs, ms, errs) = getOpt Permute options xs
              opts = foldr ($) default_opts fs
          case errs of
            [] | show_version opts ->
                  putStrLn ("graphmod ")

               | otherwise -> do
                   (incs,inps) <- fromCabal (use_cabal opts)
                   g@(AllEdges normalE sourceE, nodes)
                     <- graph (foldr add_inc (add_current opts) incs)
                              (inps ++ map to_input ms)

                   if | mode_toposort opts ->
                        mapM_ putStrLn $ fmap (intercalate ".") $ resolveNodes nodes $ toposort $ IMap.toList (ISet.toList <$> normalE)
                        -- print $ IMap.toList (ISet.toList <$> normalE)

                      | otherwise ->
                        putStr (make_dot opts g)

            _ -> hPutStrLn stderr $
                  usageInfo "usage: graphmod MODULES/PATHS" options

data Input  = File FilePath | Module ModName
              deriving Show

-- | Guess if we have a file or a module name
to_input :: String -> Input
to_input m
  | takeExtension m `elem` suffixes = File m
  | otherwise                       = Module (splitModName m)

resolveNodes :: Nodes -> [Int] -> [[String]]
resolveNodes (_,map) xs = fromMaybe (error "Missing nodeId") . flip IMap.lookup map <$> xs

type Nodes'  = Trie.Trie String [((NodeT,String),Int)]
               -- Maps a path to:   ((node, label), nodeId)

type Nodes   = (Nodes', IMap.IntMap [String])

type Edges    = IMap.IntMap ISet.IntSet

data NodeT    = ModuleNode

              | ModuleInItsCluster
                -- ^ A module that has been relocated to its cluster

              | Redirect
                -- ^ This is not rendered. It is there to support replacing
                -- one node with another (e.g., when collapsing)

              | Deleted
                -- ^ This is not rendered, and edges to/from it are also
                -- not rendered.

              | CollapsedNode Bool
                -- ^ indicates if it contains module too.
                deriving (Show,Eq,Ord)

data AllEdges = AllEdges
  { normalEdges   :: Edges
  , sourceEdges   :: Edges
  }

noEdges :: AllEdges
noEdges = AllEdges { normalEdges    = IMap.empty
                   , sourceEdges    = IMap.empty
                   }

combs :: Int -> [([Int], [Int])] -> [[([Int], [Int])]]
combs 0 _ = [[]]
combs _ [] = []
combs k (x:xs) = ((x :) <$> combs (k - 1) xs) ++ combs k xs

toposort :: [(Int, [Int])] -> [Int]
toposort xs
  | (not . null) cycleDetect =
    error $ "Dependency cycle detected for nodes " ++ show cycleDetect
  | otherwise = foldl makePrecede [] dB
  where
    dB :: [([Int], [Int])]
    dB = ((\(x, y) -> (x, y \\ x)) . (return *** id)) <$> xs
    makePrecede :: [Int] -> ([Int], [Int]) -> [Int]
    makePrecede ts ([x], xs) =
      nub $
      case elemIndex x ts of
        Just i -> uncurry (++) $ first (++ xs) $ splitAt i ts
        _ -> ts ++ xs ++ [x]
    cycleDetect :: [[Int]]
    cycleDetect =
      filter ((> 1) . length) $
      (\[(a, as), (b, bs)] -> (a `intersect` bs) ++ (b `intersect` as)) <$>
      combs 2 dB

graph :: Opts -> [Input] -> IO (AllEdges, Nodes)
graph opts inputs = fmap maybePrune $ mfix $ \ ~(_,mods) ->
  -- NOTE: 'mods' is the final value of 'done' in the funciton 'loop'.

  let nodeFor x         = lookupMod x mods    -- Recursion happens here!

      loop :: Nodes ->
              AllEdges    {- all kinds of edges -} ->
              Int         {- size -} ->
              [Input]     {- root files/modules -} ->
              IO (AllEdges, Nodes)

      loop (done', dict) es _ [] =
              return (es, (collapseAll opts done' (collapse_quals opts), dict))

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


  in loop (Trie.empty, mempty) noEdges 0 inputs

  where
  maybePrune (es,ns)
    | prune_edges opts  = (es { normalEdges = pruneEdges (normalEdges es) }, ns)
    | otherwise         = (es,ns)

  ignore done m         = isIgnored (ignore_mods opts) m
                       || isJust (lookupMod m done)




lookupMod :: ModName -> Nodes -> Maybe Int
lookupMod (q,m) (t,_) = (msum . map isThis =<< Trie.lookup q t)
  where isThis ((ty,m'),nid) =
          case ty of
            CollapsedNode False -> Nothing -- Keep looking for the actual node
            Deleted             -> Nothing
            _                   -> guard (m == m') >> return nid

insMod :: ModName -> Int -> Nodes -> Nodes
insMod (q,m) n (t,d)  = (Trie.insert q ins t,IMap.insert n (q ++ [m]) d)
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


-- XXX: We could combine collapseAll and collapse into a single pass
-- to avoid traversing form the root each time.
collapseAll :: Opts -> Nodes' -> Trie.Trie String Bool -> Nodes'
collapseAll opts t0 =
  foldr (\q t -> fromMaybe t (collapse opts t q)) t0 . toList
  where
  toList (Trie.Sub _ (Just x))  = return ([], x)
  toList (Trie.Sub as Nothing)  = do (q,t)  <- Map.toList as
                                     (qs,x) <- toList t
                                     return (q:qs, x)

-- NOTE: We use the Maybe type to indicate when things changed.
collapse :: Opts -> Nodes' -> (Qualifier,Bool) -> Maybe Nodes'
collapse _ _ ([],_) = return (Trie.empty)      -- Probably not terribly useful.

collapse opts (Trie.Sub ts mb) ([q],alsoMod) =
  do t   <- Map.lookup q ts
     let will_move = mod_in_cluster opts && Map.member q ts
         (thisMod,otherMods)
            | alsoMod || will_move = case findThisMod =<< mb of
                                       Nothing         -> (Nothing, [])
                                       Just (nid,rest) -> (Just nid, rest)
            | otherwise = (Nothing, fromMaybe [] mb)

     -- use this node-id to represent the collapsed cluster
     rep <- msum [ thisMod, getFirst t ]

     let close ((_,nm),_) = ((if will_move then Deleted else Redirect,nm),rep)
         ts'              = Map.insert q (fmap (map close) t) ts
         newT | alsoMod || not will_move = CollapsedNode (isJust thisMod)
              | otherwise                = ModuleNode

     return (Trie.Sub ts' (Just (((newT,q),rep) : otherMods)))
  where
  findThisMod (((_,nm),nid) : more) | nm == q = Just (nid,more)
  findThisMod (x : more) = do (yes,more') <- findThisMod more
                              return (yes, x:more')
  findThisMod []         = Nothing

  getFirst (Trie.Sub ts1 ms) =
    msum (fmap snd (listToMaybe =<< ms) : map getFirst (Map.elems ts1))

collapse opts (Trie.Sub ts ms) (q : qs,x) =
  do t <- Map.lookup q ts
     t1 <- collapse opts t (qs,x)
     return (Trie.Sub (Map.insert q t1 ts) ms)



-- | If inside cluster A.B we have a module M,
-- and there is a cluster A.B.M, then move M into that cluster as a special node
moveModulesInCluster :: Nodes' -> Nodes'
moveModulesInCluster (Trie.Sub su0 ms0) =
  goMb (fmap moveModulesInCluster su0) ms0
  where
  goMb su mb =
    case mb of
      Nothing -> Trie.Sub su Nothing
      Just xs -> go [] su xs

  go ns su xs =
    case xs of
      [] -> Trie.Sub su $ if null ns then Nothing else Just ns
      y : ys ->
        case check y su of
          Left it   -> go (it : ns) su ys
          Right su1 -> go ns su1 ys

  check it@((nt,s),i) mps =
    case nt of
      ModuleNode ->
        case Map.lookup s mps of
          Nothing -> Left it
          Just t  -> Right (Map.insert s (Trie.insert [] add t) mps)
            where
            newM   = ((ModuleInItsCluster,s),i)
            add xs = [newM] ++ fromMaybe [] xs


      ModuleInItsCluster    -> Left it
      CollapsedNode _       -> Left it
      Redirect              -> Left it
      Deleted               -> Left it


-- We use tries to group modules by directory.
--------------------------------------------------------------------------------



-- Render edges and a trie into the dot language
--------------------------------------------------------------------------------
make_dot :: Opts -> (AllEdges,Nodes) -> String
make_dot opts (es,t@(t',dict)) =
  showDot $
  do attribute ("size", graph_size opts)
     attribute ("ratio", "fill")
     let cols = colors (color_scheme opts)
     if use_clusters opts
        then make_clustered_dot cols $
               if mod_in_cluster opts then moveModulesInCluster t' else t'
        else make_unclustered_dot cols "" t' >> return ()
     genEdges normalAttr (normalEdges es)
     genEdges sourceAttr (sourceEdges es)
  where
  normalAttr _x _y  = []
  sourceAttr _x _y  = [("style","dashed")]

  genEdges attr edges =
    forM_ (IMap.toList edges) $ \(x,ys) ->
      forM_ (ISet.toList ys) $ \y ->
        edge (userNodeId x) (userNodeId y) (attr x y)






make_clustered_dot :: [Color] -> Nodes' -> Dot ()
make_clustered_dot cs0 su = go (0,0,0) cs0 su >> return ()
  where
  clusterC = "#0000000F"

  go outer_col ~(this_col:more) (Trie.Sub xs ys) =
    do let outerC = renderColor outer_col
           thisC  = renderColor this_col

       forM_ (fromMaybe [] ys) $ \((t,ls),n) ->
         unless (t == Redirect || t == Deleted) $
         userNode (userNodeId n) $
         [ ("label",ls) ] ++
         case t of
           CollapsedNode False ->   [ ("shape", "box")
                                    , ("style","filled")
                                    , ("color", clusterC)
                                    ]
           CollapsedNode True    -> [ ("style","filled")
                                    , ("fillcolor", clusterC)
                                    ]
           ModuleInItsCluster    -> [ ("style","filled,bold")
                                    , ("fillcolor", outerC)
                                    ]

           ModuleNode            -> [ ("style", "filled")
                                    , ("fillcolor", thisC)
                                    , ("penwidth","0")
                                    ]
           Redirect              -> []
           Deleted               -> []
       goSub this_col more (Map.toList xs)

  goSub _ cs [] = return cs
  goSub outer_col cs ((name,sub) : more) =
    do (_,cs1) <- cluster $ do attribute ("label", name)
                               attribute ("color" , clusterC)
                               attribute ("style", "filled")
                               go outer_col cs sub

       goSub outer_col cs1 more


make_unclustered_dot :: [Color] -> String -> Nodes' -> Dot [Color]
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
           CollapsedNode False   -> [ ("shape", "box"), ("color", col) ]
           CollapsedNode True    -> [ ("shape", "box") ]
           Redirect              -> []
           ModuleInItsCluster    -> []
           ModuleNode            -> []
           Deleted               -> []

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
                              `X.catch` \SomeException {} -> return []
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

  , mode_toposort :: Bool
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
  , mode_toposort   = False
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

  , Option ['t'] ["toposort"] (NoArg set_mode_toposort)
    "Print the module list in dependency-toposorted order."
  ]

add_current      :: OptT
add_current o     = case inc_dirs o of
                      [] -> o { inc_dirs = ["."] }
                      _  -> o

set_mode_toposort  :: OptT
set_mode_toposort o = o { mode_toposort = True }

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
