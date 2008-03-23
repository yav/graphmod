module Data.Dot where

data DotGraph = DotGraph [GraphElement]
	deriving Show

type NodeId = String

data GraphElement = GraphAttribute String String
		  | GraphNode NodeId        [(String,String)]
		  | GraphEdge NodeId NodeId [(String,String)]
		  | Scope           [GraphElement]
		  | SubGraph NodeId [GraphElement]
	deriving Show

data DM a = DM { unDM :: Int -> ([GraphElement],Int,a) }

instance Monad DM where
  return a = DM $ \ uq -> ([],uq,a)
  m >>= k  = DM $ \ uq -> case unDM m uq of
			   (g1,uq',r) -> case unDM (k r) uq' of
					   (g2,uq2,r2) -> (g1 ++ g2,uq2,r2)

node      :: [(String,String)] -> DM NodeId
node attrs = DM $ \ uq -> let nid = "n" ++ show uq 
			  in ( [ GraphNode nid attrs ],succ uq,nid)

named_node :: NodeId -> [(String,String)] -> DM ()
named_node nid attrs = DM $ \ uq -> ( [ GraphNode nid attrs ], uq, ())

edge      :: NodeId -> NodeId -> [(String,String)] -> DM ()
edge  from to attrs = DM (\ uq -> ( [ GraphEdge from to attrs ],uq,()))

(.->.) from to = edge from to []

scope     :: DM a -> DM a
scope (DM fn) = DM (\ uq -> case fn uq of
			      ( elems,uq',a) -> ([Scope elems],uq',a))

cluster   :: String -> DM a -> DM (NodeId,a)
cluster name (DM fn) = DM (\ uq -> 
		let cid = "cluster_" ++ show uq 
		in case fn (succ uq) of
		    (elems,uq',a) -> ([SubGraph cid elems],uq',(cid,a)))

attribute :: String -> String -> DM ()
attribute name val = DM (\ uq -> ( [  GraphAttribute name val ],uq,()))

showDot :: DM () -> String
showDot (DM dm) = case dm 0 of
		    (elems,_,()) -> "digraph G {\n" ++ unlines (map showGraphElement elems) ++ "\n}\n"

showGraphElement (GraphAttribute name val) = showAttr (name,val) ++ ";"
showGraphElement (GraphNode nid attrs)           = nid ++ "[" ++ showAttrs attrs ++ "];"
showGraphElement (GraphEdge from to attrs) = from ++ " -> " ++ to ++ "[" ++ showAttrs attrs ++ "];"
showGraphElement (Scope elems) = "{\n" ++ unlines (map showGraphElement elems) ++ "\n}"
showGraphElement (SubGraph nid elems) = "subgraph " ++ nid ++ " {\n" ++ unlines (map showGraphElement elems) ++ "\n}"

showAttrs [] = []
showAttrs [a]    = showAttr a
showAttrs (a:as) = showAttr a ++ "," ++ showAttrs as

showAttr (name,val) = name ++ "=" ++ show val
