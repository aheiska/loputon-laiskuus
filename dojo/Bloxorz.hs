module Bloxorz where

import Data.Set (Set, notMember, union, fromList, singleton, insert, empty)
import Levels
import Terrain
import Block

type Explored = Set Block
type State = (Block, [Move])

-- TODO 8:
-- Returns `true` if the block `b` is at the final position
done :: Level -> Block ->  Bool
done level block =
  standing block &&
  p1 block == goal level

-- TODO 9:
-- This function takes three arguments: the current block `b`,
-- a list of moves `history` that was required to reach the
-- position of `b` and the level that is played.
--
-- The `head` element of the `history` list is the latest move
-- that was executed, i.e. the last move that was performed for
-- the block to end up at position `b`.
--
-- The function returns a list of pairs: the first element of
-- the each pair is a neighboring block, and the second element
-- is the augmented history of moves required to reach this block.
--
-- It should only return valid neighbors, i.e. block positions
-- that are inside the terrain.
neighboursWithHistory :: Level -> Block -> [Move] -> [State]
neighboursWithHistory level block history =
  map (\(b, m) -> (b, m : history)) legal
  where
    legal = legalNeighbours block (terr level)

-- TODO 10:
-- This function returns the list of neighbors without the block
-- positions that have already been explored. We will use it to
-- make sure that we don't explore circular paths.
newNeighbours :: [State] -> Explored -> [State]
newNeighbours neighbours explored =
  filter (\(b, _) -> b `notMember` explored) neighbours

-- TODO 11:
-- The function `from` returns the infinite list of all possible paths
-- that can be followed, starting at the `head` of the `initial`
-- list.
--
-- The blocks in the list `initial` are sorted by ascending path
-- length: the block positions with the shortest paths (length of
-- move list) are at the head of the list.
--
-- The parameter `explored` is a set of block positions that have
-- been visited before, on the path to any of the blocks in the
-- list `initial`. When search reaches a block that has already
-- been explored before, that position should not be included a
-- second time to avoid circles.
--
-- The resulting list should be sorted by ascending path length,
-- i.e. the block positions that can be reached with the fewest
-- amount of moves should appear first in the list.
--
-- Note: the solution should not look at or compare the lengths
-- of different paths - the implementation should naturally
-- construct the correctly sorted list.
from :: Level -> [State] -> Explored -> [State]
from _     []                         _        = []
from level (state @ (b, ms) : states) explored =
  state : from level newStates newExplored
  where
    newStates = states ++ newNeighbours (neighboursWithHistory level b ms) explored
    newBlocks = fromList (map fst newStates)
    newExplored = explored `union` newBlocks

-- TODO 12:
-- The (possibly) infinite list of all paths that begin at the starting block.
pathsFromStart :: Level -> [State]
pathsFromStart level @ Level { start = startPos } =
  from level [(startBlock, [])] (singleton startBlock)
  where
    startBlock = makeBlock startPos startPos

-- TODO 13:
-- Returns a list of all possible pairs of the goal block along
-- with the history how it was reached.
pathsToGoal :: Level -> [State]
pathsToGoal level = filter (done level . fst) (pathsFromStart level)
--pathsToGoal level = filter (\(b, _) -> done level b) (pathsFromStart level)

-- TODO 14:
-- The (or one of the) shortest sequence(s) of moves to reach the
-- goal. If the goal cannot be reached, the empty list is returned.
--
-- Note: the `head` element of the returned list should represent
-- the first move that the player should perform from the starting
-- position.
solution :: Level -> [Move]
solution level =
  case result of
    []     -> []
    p : _ -> reverse p
  where
    result = map snd (pathsToGoal level)
-- solution level = if null result then [] else reverse $ head result
--  where
--    result = map snd (pathsToGoal level)
