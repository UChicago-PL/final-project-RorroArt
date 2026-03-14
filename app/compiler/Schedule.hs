module Schedule
  ( schedule
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl')

import Machine

-- | Reorder machine ops to improve bundling density.
--   Preserves data dependencies while scheduling latency-critical
--   chains first (longest-path priority, most-constrained engine tiebreak).
schedule :: [MachineOp] -> [MachineOp]
schedule [] = []
schedule ops =
  let n      = length ops
      arr    = IM.fromList (zip [0..] ops)
      succs  = buildDAG n arr          -- i -> set of successors
      preds  = invertDAG n succs       -- i -> set of predecessors
      nPreds = IM.fromList [(i, IS.size (IM.findWithDefault IS.empty i preds))
                           | i <- [0..n-1]]
      depth  = computeDepths n succs   -- longest path to any sink
      ready0 = IS.fromList [i | i <- [0..n-1],
                            IM.findWithDefault 0 i nPreds == 0]
  in  map (arr IM.!) (topoSort arr depth ready0 nPreds succs [])

-- Engine priority for tie-breaking (higher = picked first = most constrained).
enginePrio :: MachineSlot -> Int
enginePrio (MSlotFlow  _) = 5
enginePrio (MSlotStore _) = 4
enginePrio (MSlotLoad  _) = 3
enginePrio (MSlotValu  _) = 2
enginePrio (MSlotAlu   _) = 1

-- Build successor adjacency: edge i->j means j depends on i.
buildDAG :: Int -> IntMap MachineOp -> IntMap IntSet
buildDAG n arr =
  let pairs = [(i, j) | i <- [0..n-2], j <- [i+1..n-1],
                         depends (arr IM.! i) (arr IM.! j)]
  in  foldl' (\m (i,j) -> IM.insertWith IS.union i (IS.singleton j) m)
             IM.empty pairs

-- Does op B depend on op A?
depends :: MachineOp -> MachineOp -> Bool
depends a b = raw || waw || war || memDep
  where
    aw = moWrites a; bw = moWrites b
    ar = moReads  a; br = moReads  b
    raw = any (`elem` br) aw
    waw = any (`elem` bw) aw
    war = any (`elem` bw) ar
    memDep = any (\ma -> any (memConflict ma) (moMem b)) (moMem a)

memConflict :: MemAccess -> MemAccess -> Bool
memConflict a b =
  maRegion a == maRegion b
  && (maKind a == "store" || maKind b == "store")
  && overlap (maRange a) (maRange b)

overlap :: MemRange -> MemRange -> Bool
overlap Unknown _                     = True
overlap _ Unknown                     = True
overlap (Range a0 a1) (Range b0 b1)   = not (a1 < b0 || b1 < a0)

-- Invert the DAG: build predecessor sets from successor sets.
invertDAG :: Int -> IntMap IntSet -> IntMap IntSet
invertDAG _ succs =
  IM.foldlWithKey' (\m i js ->
      IS.foldl' (\m' j -> IM.insertWith IS.union j (IS.singleton i) m') m js
    ) IM.empty succs

-- Compute longest path from each node to any sink (reverse topo BFS).
computeDepths :: Int -> IntMap IntSet -> IntMap Int
computeDepths n succs =
  let -- Process nodes in reverse order; a node's depth = 1 + max successor depth.
      go i dm
        | IS.null js = IM.insert i 0 dm
        | otherwise  = IM.insert i (1 + IS.foldl' (\mx j -> max mx (IM.findWithDefault 0 j dm)) 0 js) dm
        where js = IM.findWithDefault IS.empty i succs
  in  foldr go IM.empty [0..n-1]

-- Greedy topological sort: always pick the ready op with highest priority.
topoSort :: IntMap MachineOp -> IntMap Int -> IntSet -> IntMap Int
          -> IntMap IntSet -> [Int] -> [Int]
topoSort arr depth ready nPreds succs acc
  | IS.null ready = reverse acc
  | otherwise =
      let best = pickBest arr depth ready
          js   = IM.findWithDefault IS.empty best succs
          (ready', nPreds') = IS.foldl' (decrement best) (IS.delete best ready, nPreds) js
      in  topoSort arr depth ready' nPreds' succs (best : acc)

-- Decrement predecessor count; if it hits zero the node becomes ready.
decrement :: Int -> (IntSet, IntMap Int) -> Int -> (IntSet, IntMap Int)
decrement _ (rdy, np) j =
  let c = IM.findWithDefault 1 j np - 1
      np' = IM.insert j c np
  in  if c == 0 then (IS.insert j rdy, np') else (rdy, np')

-- Pick the ready node with (1) longest depth, (2) highest engine priority.
pickBest :: IntMap MachineOp -> IntMap Int -> IntSet -> Int
pickBest arr depth ready =
  IS.foldl' (\best i ->
      let dI = IM.findWithDefault 0 i depth
          dB = IM.findWithDefault 0 best depth
          eI = enginePrio (moSlot (arr IM.! i))
          eB = enginePrio (moSlot (arr IM.! best))
      in  if dI > dB || (dI == dB && eI > eB)
            then i else best
    ) (IS.findMin ready) ready
