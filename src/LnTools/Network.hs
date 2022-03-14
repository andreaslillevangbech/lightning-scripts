{-# LANGUAGE LambdaCase #-}

module LnTools.Network (
    GraphWithPath,
    Path (..),
    parseChannels,
    getGraph,
) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.Word (Word64)
import LnTools (Channel (..), NodeId)

-- Gives you one direction of a channel, so you have two of these for each channel
data OneDirection = OneDirection
    { channel :: Channel
    , direction :: NodeId
    } deriving (Eq, Ord)

-- Adjacency map
type Adj = Map NodeId (Set OneDirection)

-- Given a list of channels, split each channel into two directions
parseChannels :: [Channel] -> Adj
parseChannels = foldl' (flip insertChannel) mempty
    where
        insertChannel chan = 
            (insert <$> node1 <*> dir1) chan
            . (insert <$> node2 <*> dir2) chan
        dir1 = OneDirection <$> id <*> node2
        dir2 = OneDirection <$> id <*> node1
        insert k v = M.insertWith (<>) k (S.singleton v)

-- A path is either a path to the node itself or a path to anther node with
-- a jump count and a capacity
data Path = PathToSelf | Path Int Word64 deriving (Show)

-- GraphWithPath is the annotated graph containing for 
-- each node all the nodes it has a path to
type GraphWithPath = Map NodeId (Map NodeId Path)

-- The list of nodeid's are the nodes you wish to pay.
-- From that the graph is expanded with updateGraphStep using the channels available
initialGraph :: [NodeId] -> GraphWithPath
initialGraph ns = M.fromList $ (\n -> (n, M.singleton n PathToSelf)) <$> ns

-- Our final graph with all the available paths
getGraph :: Int -> Adj -> [NodeId] -> GraphWithPath
getGraph maxSteps adjs =
    (!! maxSteps) . iterate (updateGraphStep adjs) . initialGraph

-- Call updateGraph iteratively on the initialGraph. Each time you call you add one more
-- jump from the payees that you are looking to pay. A max number of jumps should be
-- an option in the cli args
updateGraphStep :: Adj -> GraphWithPath -> GraphWithPath
updateGraphStep adjs graph0 = foldl' insertWithNode graph0 newNodes
    where
        visited = M.keys graph0
        newNodes = 
            (`S.difference` S.fromList visited)
            . S.map direction 
            . S.unions 
            . mapMaybe (`M.lookup` adjs) 
            $ visited

        insertWithNode :: GraphWithPath -> NodeId -> GraphWithPath
        insertWithNode graph node = foldl' updateWithChannel graph $ S.map channel adj
            where Just adj = M.lookup node adjs

        -- Use the channel to update all the path data in the graph
        updateWithChannel :: GraphWithPath -> Channel -> GraphWithPath
        updateWithChannel graph chan = 
            M.insert sourceNode (updatePath chan sourcePath destPath)
            . M.insert destNode (updatePath chan destPath sourcePath)
            $ graph
            where
                sourceNode = node1 chan
                sourcePath = M.findWithDefault mempty sourceNode graph
                destNode = node2 chan
                destPath = M.findWithDefault mempty destNode graph

        updatePath :: 
            Channel ->
            -- node
            Map NodeId Path ->
            -- neighbor node
            Map NodeId Path -> 
            Map NodeId Path
        updatePath chan nodePaths neighborPaths =
            M.differenceWith (comparePath chan) nodePaths neighborPaths
            `M.union` M.map (extendPath chan) neighborPaths

        -- Bit of a hack to return a Maybe, but it is required by differenceWith
        -- Path with highest capacity
        comparePath :: Channel -> Path -> Path -> Maybe Path
        comparePath _ PathToSelf _ = Just PathToSelf
        comparePath chan nodePath@(Path _ nodeCap) neighborPath = case neighborPath of
          Path neighborHops neighborCap -> if newCap > nodeCap
              then Just $ Path (neighborHops + 1) newCap
              else Just nodePath
                where
                    newCap = min (capacity chan) neighborCap
          -- case of it being a path to the neighbor itself
          PathToSelf -> if capacity chan > nodeCap
                         then Just $ Path 1 (capacity chan)
                         else Just nodePath

        -- If the node does not have a path to another node, but the 
        -- neighbor does, then there now is a path with the channel
        extendPath :: Channel -> Path -> Path
        extendPath chan = \case
           PathToSelf ->  Path 1 (capacity chan)
           Path hops cap -> Path (hops + 1) $ min (capacity chan) cap
            
