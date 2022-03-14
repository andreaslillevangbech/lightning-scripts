-- Isolate the best nodes
module LnTools.Analysis
  ( shortestPath
  , bestConnected
  , bestCapacity
  ) where

import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Ord ( Down(Down) )
import LnTools (NodeId, Payee (..))
import LnTools.Network (Path (..))

-- Only keep nodes that belong to the argmax given the measure
includeByMeasure :: (Ord k, Ord v) => (a -> v) -> v -> Map k a -> Map k a
includeByMeasure measure init = snd . M.foldrWithKey includeIf (init, mempty)
  where
    includeIf nodeId nodePaths acc@(n, out)
      | m > n = (m, M.singleton nodeId nodePaths)
      | m == n = (n, M.insert nodeId nodePaths out)
      | otherwise = acc
      where
        m = measure nodePaths

-- Compare lengths of weighted sum of paths
data PathLength = NoPath | PathLength Double
  deriving (Eq, Show)

instance Ord PathLength where
  compare NoPath _ = GT
  compare _ NoPath = LT
  compare (PathLength l1) (PathLength l2) = compare l1 l2

-- By weight, get the nodes with the combined shortest path to all payees (shortest by weighted jumps)
shortestPath ::
     [Payee] -> Map NodeId (Map NodeId Path) -> Map NodeId (Map NodeId Path)
shortestPath payees = includeByMeasure weightedLength (Down NoPath)
  where
    weightedLength nodePaths =
      -- If there is no path to one of the payees then dont include
      Down $
      case mapM (`M.lookup` nodePaths) $ node <$> payees of
        Nothing -> NoPath
        Just paths ->
          PathLength . sum $ zipWith pathLength (weight <$> payees) paths
    pathLength _ PathToSelf = 0
    pathLength w (Path h _) = w * fromIntegral h / weightsTotal
    weightsTotal = sum $ weight <$> payees

-- Only keep nodes with paths to the maximum number of other nodes
bestConnected :: Map NodeId (Map NodeId Path) -> Map NodeId (Map NodeId Path)
bestConnected = includeByMeasure M.size 1

-- If a any node has better capacity path to all nodes than another node, then
-- remove the weak node. Of course don't measure capacity of path to self
bestCapacity :: Map NodeId (Map NodeId Path) -> Map NodeId (Map NodeId Path)
bestCapacity = M.foldrWithKey insertIf mempty
  where
    insertIf nodeId nodePaths acc
      | null acc = M.singleton nodeId nodePaths
      | not $ any (nodePaths `dominatedBy`) acc =
        M.insert nodeId nodePaths $
        M.filter (not . (`dominatedBy` nodePaths)) acc
      | otherwise = acc

-- NOTE: should you exclude the path to self cases?
dominatedBy :: Map NodeId Path -> Map NodeId Path -> Bool
dominatedBy paths = and . M.intersectionWith dominates paths
  where
    dominates PathToSelf _ = False
    dominates _ PathToSelf = True
    dominates (Path n1 c1) (Path n2 c2) = c2 > c1 || (c2 == c1 && n2 <= n1)
