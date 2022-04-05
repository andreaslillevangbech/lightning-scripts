{-# LANGUAGE LambdaCase #-}

module LnTools.Network (
    GraphWithPath,
    Graph (..),
    Path (..),
    parseChannels,
    getGraph,
) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Map.Merge.Lazy as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Foldable (foldl', toList)
import Data.Word (Word64)
import LnTools (Channel (..), NodeId)

import Debug.Trace

-- Gives you one direction of a channel, so you have two of these for each channel
data OneDirection = OneDirection
    { channel :: Channel
    , direction :: NodeId
    } deriving (Eq, Ord, Show)

-- Adjacency map
type Adj = Map NodeId [OneDirection]

-- Given a list of channels, split each channel into two directions
parseChannels :: [Channel] -> Adj
parseChannels = foldl' (flip insertChannel) mempty
    where
        insertChannel chan = 
            (insert <$> source <*> dir1) chan
            . (insert <$> destination <*> dir2) chan
        dir1 = OneDirection <$> id <*> destination
        dir2 = OneDirection <$> id <*> source
        insert k v = M.insertWith (<>) k [v]

-- A path is either a path to the node itself or a path to anther node with
-- a jump count and a capacity
data Path = PathToSelf | Path Int Word64 deriving (Show, Eq)

-- GraphWithPath is the annotated graph containing for 
-- each node all the nodes it has a path to
type GraphWithPath = Map NodeId (Map NodeId Path)

data Graph = Graph
    { theGraph :: GraphWithPath
    , visitedNodes :: Set NodeId 
    } deriving (Eq, Show)

-- The list of nodeid's are the nodes you wish to pay.
-- From that the graph is expanded with updateGraphStep using the channels available
initialGraph :: [NodeId] -> Graph
initialGraph ns = 
    Graph
        { theGraph = M.fromList $ (\n -> (n, M.singleton n PathToSelf)) <$> ns
        , visitedNodes = S.fromList ns
        }

-- Our final graph with all the available paths
-- Takes the nodes you want to pay as input
getGraph :: Int -> Adj -> [NodeId] -> Graph
getGraph maxSteps adjs =
    (!! maxSteps) . iterate (updateGraphStep adjs) . initialGraph

-- Call updateGraph iteratively on the initialGraph. Each time you call you add one more
-- jump from the payees that you are looking to pay. A max number of jumps should be
-- an option in the cli args
updateGraphStep :: Adj -> Graph -> Graph
updateGraphStep adjs graph0 = trace ("Number of nodes: " ++ show (length visited))
    $ Graph
        { theGraph = foldl' insertWithNode (theGraph graph0) newNodes
        , visitedNodes = visited <> newNodes
        }
    where
        visited = visitedNodes graph0
        newNodes = 
            S.fromList
                . fmap direction
                . mconcat
                . mapMaybe (`M.lookup` adjs) 
                $ toList visited

        insertWithNode :: GraphWithPath -> NodeId -> GraphWithPath
        insertWithNode graph node 
            | Just adj <- M.lookup node adjs = 
                foldl' updateWithChannel graph $ map channel adj
            | otherwise = graph

        -- Use the channel to update all the path data in the graph
        updateWithChannel :: GraphWithPath -> Channel -> GraphWithPath
        updateWithChannel graph chan = 
            -- trace ("Path update: " ++ show sourcePath ++ show newSourcePath) $
            M.insert sourceNode newSourcePath
            . M.insert destNode (updatePath chan destPath sourcePath)
            $ graph
            where
                sourceNode = source chan
                sourcePath = M.findWithDefault mempty sourceNode graph
                destNode = destination chan
                destPath = M.findWithDefault mempty destNode graph
                newSourcePath = updatePath chan sourcePath destPath

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
            
