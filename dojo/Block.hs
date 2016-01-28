module Block where

import Control.Exception (assert)
import Terrain
import Prelude hiding (Right, Left)

-- In Bloxorz, we can move left, right, Up or down.
-- These moves are encoded as constructors for Move data type.
data Move = Up | Down | Left | Right deriving (Eq, Show)

-- A block is represented by the position of the two cubes that
-- it consists of.
data Block = B { p1 :: Pos, p2 :: Pos } deriving (Ord, Eq, Show)

-- We make sure that `b1` is lexicographically
-- smaller than `b2`.
makeBlock :: Pos -> Pos -> Block
makeBlock p1 p2 = assert (x p1 <= x p2 && y p1 <= y p2) $ B p1 p2

-- The position obtained by changing the `y` coordinate by `d`
dy :: Pos -> Int -> Pos
dy pos d = pos { y = y pos + d }

-- The position obtained by changing the `x` coordinate by `d`
dx :: Pos -> Int -> Pos
dx pos d = pos { x = x pos + d }

-- Returns a block where the `y` coordinates of `p1` and `p2` are
-- changed by `d1` and `d2`, respectively.
blockDy :: Block -> Int -> Int -> Block
blockDy b d1 d2 = makeBlock (dy (p1 b) d1) (dy (p2 b) d2)

-- Returns a block where the `x` coordinates of `p1` and `p2` are
-- changed by `d1` and `d2`, respectively.
blockDx :: Block -> Int -> Int -> Block
blockDx b d1 d2 = makeBlock (dx (p1 b) d1) (dx (p2 b) d2)

-- TODO 3:
-- Returns `True` if the block is standing.
standing :: Block -> Bool
standing (B pa pb) = pa == pb

horizontal :: Block -> Bool
horizontal block = (x $ p1 block) == (x $ p2 block)

-- TODO 4:
-- Returns `True` if the block is entirely inside the terrain.
isLegal :: Terrain -> Block ->  Bool
isLegal terrain block = terrain (p1 block) && terrain (p2 block)

-- TODO 5:
-- This function returns the block at the start position of
-- the game.
startBlock :: Level -> Block
startBlock level = makeBlock s s
  where
    s = start level

-- Functions left, right, up and down encode block moves
-- in Bloxorz game.

left :: Block -> Block
left block
  | standing block   = blockDy block (-2) (-1)
  | horizontal block = blockDy block (-1) (-2)
  | otherwise        = blockDy block (-1) (-1)

right :: Block -> Block
right block
  | standing block   = blockDy block 1 2
  | horizontal block = blockDy block 2 1
  | otherwise        = blockDy block 1 1

up :: Block -> Block
up block
  | standing block   = blockDx block (-2) (-1)
  | horizontal block = blockDx block (-1) (-1)
  | otherwise        = blockDx block (-1) (-2)

down :: Block -> Block
down block
  | standing block   = blockDx block 1 2
  | horizontal block = blockDx block 1 1
  | otherwise        = blockDx block 2 1

-- TODO 6:
-- Returns the list of blocks that can be obtained by moving
-- the current block, together with the corresponding move.
neighbours :: Block -> [(Block, Move)]
neighbours block = [(left block, Left), (right block, Right),
  (up block, Up), (down block, Down)]

-- TODO 7:
-- Returns the list of positions reachable from the current block
-- which are inside the terrain.
legalNeighbours :: Block -> Terrain -> [(Block, Move)]
legalNeighbours block terrain = filter ((isLegal terrain) . fst) (neighbours block)
--legalNeighbours block terrain = filter (\(b, _) -> isLegal terrain b) (neighbours block)
