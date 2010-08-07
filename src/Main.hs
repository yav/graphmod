import Utils
import qualified Trie

import Text.Dot

import Control.Monad(when,forM_,(<=<),mplus,msum,guard)
import Control.Monad.Fix(mfix)
import Data.List(intersperse,partition)
import Data.Maybe(mapMaybe,isJust,fromMaybe,listToMaybe)
import qualified Data.IntMap as IMap
import qualified Data.Map    as Map
import qualified Data.IntSet as Set
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)
import System.FilePath
import System.Console.GetOpt
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
                  do g <- graph (add_current opts) (map to_input ms)
                     putStrLn (make_dot (use_clusters opts) g)
              where opts = foldr ($) default_opts fs

            _ -> hPutStrLn stderr $
                  usageInfo "usage: graphmod MODULES/PATHS" options


data Input  = File FilePath | Module ModName

-- | Guess if we have a file or a module name
to_input :: String -> Input
to_input m
  | takeExtension m `elem` suffixes = File m
  | otherwise                       = Module (splitModName m)



-- type Nodes    = Trie.Trie String [(String,Int)]
type NodesC   = Trie.Trie String [((NodeT,String),Int)]
type Edges    = IMap.IntMap Set.IntSet

data NodeT    = ModuleNode
              | CollapsedNode Bool  -- ^ indicates if it contains module too
                deriving (Show,Eq,Ord)

graph :: Opts -> [Input] -> IO (Edges, NodesC)
graph opts inputs = mfix $ \ ~(_,mods) ->
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
  (lookupBy (isCollapsed q) =<< mb) `mplus`
  (lookupNode (qs,m) =<< Map.lookup q ts)

containsModule :: String -> (NodeT, String) -> Bool
containsModule q (ModuleNode, q1)             = q == q1
containsModule q (CollapsedNode withMod, q1)  = withMod && q == q1

isCollapsed :: String -> (NodeT, String) -> Bool
isCollapsed q (CollapsedNode _, q1) = q == q1
isCollapsed _ (ModuleNode, _)       = False

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
            `mplus` fmap (\x -> (x,False)) (getFirst =<< Map.lookup q ts)

     return $ Trie.Sub (Map.delete q ts)
            $ Just $ ((CollapsedNode withMod,q),n)
                   : if withMod then others else allNodes

  where allNodes          = fromMaybe [] mb
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
make_dot :: Bool -> (Edges,NodesC) -> String
make_dot cl (es,t) =
  showDot $
  do if cl then make_clustered_dot 0 t
           else make_unclustered_dot 0 "" t >> return ()
     forM_ (IMap.toList es) $ \(x,ys) ->
       forM_ (Set.toList ys) $ \y -> userNodeId x .->. userNodeId y



pickLabel l (CollapsedNode False) = [("label", l)]
pickLabel l (CollapsedNode True)  = [("label", l ++ " *")]
pickLabel l ModuleNode            = [("label", l)]

pickShape (CollapsedNode _ ) = [ ("shape", "folder") ]
pickShape ModuleNode         = []


make_clustered_dot :: Int -> NodesC -> Dot ()
make_clustered_dot c (Trie.Sub xs ys) =
  do forM_ (fromMaybe [] ys) $ \((t,ls),n) ->
       userNode (userNodeId n) $
        pickLabel ls t ++ pickShape t ++
       case t of
         CollapsedNode _ -> [ ("color",colors !! c), ("style","filled") ]
         ModuleNode      -> []
     forM_ (Map.toList xs) $ \(name,sub) ->
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
            pickLabel (pre ++ ls) t ++ pickShape t
            ++ [ ("fillcolor", colors !! c)
               , ("style", "filled")
               ]
     let c1 = if null ys then c else c + 1
     c1 `seq` loop (Map.toList xs) c1
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
  , collapse_quals :: Trie.Trie String Bool
    -- ^ The "Bool" tells us if we should collapse modules as well.
    -- For example, "True" says that A.B.C would collapse not only A.B.C.*
    -- but also the module A.B.C, if it exists.
  , show_version  :: Bool
  }

type IgnoreSet  = Trie.Trie String IgnoreSpec
data IgnoreSpec = IgnoreAll | IgnoreSome [String]  deriving Show

type OptT = Opts -> Opts

default_opts :: Opts
default_opts = Opts
  { inc_dirs      = []
  , quiet         = False
  , with_missing  = False
  , use_clusters  = True
  , ignore_mods   = Trie.empty
  , collapse_quals = Trie.empty
  , show_version    = False
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

add_collapse_qual :: Bool -> String -> OptT
add_collapse_qual m s o = o { collapse_quals = upd (splitQualifier s)
                                                      (collapse_quals o) }

  where
  upd [] (Trie.Sub xs (Just _)) = Trie.Sub xs (Just m)
  upd _ t@(Trie.Sub _ (Just _)) = t
  upd [] _                      = Trie.Sub Map.empty (Just m)
  upd (q:qs) (Trie.Sub as _)    = Trie.Sub (Map.alter add q as) Nothing
    where add j = Just $ upd qs $ fromMaybe Trie.empty j


