module UnitTests where

import Test.HUnit

import Block
import Bloxorz
import Levels
import Terrain

import Prelude hiding (Right, Left)
import Data.Set (fromList)

-- To run the tests inside the interpreter, execute "runTestTT tests"

tests = TestList
  [ TestLabel "Terrain function for level 1" test1a
  , TestLabel "Terrain function for level 1" test1b
  , TestLabel "FindChar for level 1" test2
  , TestLabel "Optional solution for level 1" test3
  , TestLabel "Optional solution length for level 1" test4
  , TestLabel "Optional solution length for an infinite level" test5
  , TestLabel "Finding neighbors for level 1" test6
  , TestLabel "Finding new neighbors for level 1" test7
  ]

test1a = TestCase (assertBool "is terrain at 0,0" (terrain levelVector (Pos 0 0)))
test1b = TestCase (assertBool "is not terrain at 4,11" (not $ terrain levelVector (Pos 4 11)))

levelVector = toTerrain level1

test2 = TestCase (assertEqual "start == 1,1" (Pos 1 1) (start level))
  where level = buildLevel level1

test3 =
    TestCase (assertEqual "optimal solution for level 1" (makeBlock g g) (solve (makeBlock s s) moves))
  where s = start level
        g = goal level
        level = buildLevel level1
        moves = solution level

test4 =
    TestCase (assertEqual "optimal solution length for level 1" (length level1solution) (length moves))
  where level = buildLevel level1
        moves = solution level

test5 =
  TestCase (assertEqual "infinite level solution length" 8 (length (solution infiniteLevel)))

test6 =
  TestCase (assertEqual
    "Neighbor history matches"
    [(makeBlock (Pos 1 2) (Pos 1 3), [Right,Left,Up]), (makeBlock (Pos 2 1) (Pos 3 1), [Down,Left,Up])]
    (neighboursWithHistory level (makeBlock (Pos 1 1) (Pos 1 1)) [Left, Up])
  )
  where level = buildLevel level1

test7 =
  TestCase (assertEqual
    "Explored neigbors are ignored"
    [(makeBlock (Pos 2 1) (Pos 3 1), [Down,Left,Up])]
    (newNeighbours
      [(makeBlock (Pos 1 2) (Pos 1 3), [Right,Left,Up]), (makeBlock (Pos 2 1) (Pos 3 1), [Down,Left,Up])]
      (fromList [makeBlock (Pos 1 2) (Pos 1 3), makeBlock (Pos 1 1) (Pos 1 1)])
      )
  )


solve :: Block -> [Move] -> Block
solve start moves = foldl follow start moves
  where follow block Left  = left block
        follow block Right = right block
        follow block Up    = up block
        follow block Down  = down block

level1solution = [Right, Right, Down, Right, Right, Right, Down]

infiniteLevel = Level (Pos 0 0) (Pos 5 5) (const True)
