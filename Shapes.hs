{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Michal Spano, Oscar Djurvall, Joel Rejholt
Lab group   : 35
-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing, isJust)
import Test.QuickCheck

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]
    
  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes] 
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (rows, cols) = Shape (replicate rows (replicate cols Nothing))

-- ** A2

-- | The size (rows and columns) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape rows) = (rowCount, colCount)
  where
    rowCount = length rows
    colCount = length (head rows)

-- ** A3

-- | Count how many non-empty squares a shape contains

blockCount :: Shape -> Int
blockCount s = length [square | square <- squares, nonEmpty square]
   where
      squares = concat (rows s)
      nonEmpty square = not (isNothing square) -- alt. `isJust` can be used

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)

prop_Shape :: Shape -> Bool
prop_Shape s
  | rowCount == 0 || colCount == 0 = False  
  | otherwise                      = isRectangular s 
  where
    (rowCount, colCount) = shapeSize s

isRectangular :: Shape -> Bool
isRectangular (Shape rows) = and [n == length r | r <- tail rows]
  where
    n = length (head rows)

-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [minBound .. maxBound]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape rows) = Shape (reverse (transpose rows))

-- Additionally, we've created a property for the `rotateShape`
-- function.
prop_rotateShape :: Shape -> Bool
prop_rotateShape shape = prop_Shape shape && idempotence4 shape
   where
      idempotence4 shape = (iterate (rotateShape) shape) !! 4 == shape

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (rows, cols) s = shiftRight cols (shiftDown rows s)

shiftDown :: Int -> Shape -> Shape
shiftDown 0 s = s
shiftDown n (Shape rows)
  | n < 0     = shiftDown (n+1) (Shape (rows ++ [newRow]))
  | otherwise = shiftDown (n-1) (Shape ([newRow] ++ rows))
  where
    (_, colCount) = shapeSize (Shape rows)
    newRow        = replicate colCount Nothing

shiftRight :: Int -> Shape -> Shape
shiftRight 0 s = s
shiftRight n (Shape rows)
  | n < 0      = shiftRight (n+1) (Shape [row ++ [Nothing] | row <- rows]) 
  | otherwise  = shiftRight (n-1) (Shape [[Nothing] ++ row | row <- rows])

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (rows, cols) = shiftShape (negate rows, negate cols)

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (rows, cols) s = padShape (padRows, padCols) s
   where
      padRows = max 0 (rows - rowCount)
      padCols = max 0 (cols - colCount)
      (rowCount, colCount) = shapeSize s

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = or $ zipWith rowsOverlap rows1 rows2
  where
    (rows1, rows2) = (rows s1, rows s2) 

rowsOverlap :: Row -> Row -> Bool
r1 `rowsOverlap` r2 = or (zipWith checkInvariant r1 r2)
  where
    checkInvariant s1 s2 = isJust s1 && isJust s2 

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith f s1 s2 = Shape (zipWith (zipWith f) r1 r2) 
  where
    (r1, r2) = (rows s1, rows s2)

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2
   | s1 `overlaps` s2 = undefined
   | otherwise        = zipShapeWith combineSquares s1' s2'
   where
      s1' = padShapeTo (max r1 r2, max c1 c2) s1
      s2' = padShapeTo (max r1 r2, max c1 c2) s2
      (r1, c1) = shapeSize s1
      (r2, c2) = shapeSize s2
      combineSquares (Just c) _   = Just c
      combineSquares _        sq2 = sq2
