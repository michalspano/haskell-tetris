{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Michal Spano, Oscar Djurvall, Joel Rejholt
Lab group   : 35
-}

module Main where

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes
import Data.Maybe (isJust)

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game 
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris 
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Piece = (Pos, Shape)
type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris 
  { piece  :: Piece    -- ^ The position and shape of the falling piece
  , well   :: Shape    -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]  -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth  = 10
wellHeight = 20

wellSize :: (Int, Int)
wellSize   = (wellHeight, wellWidth)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (0, wellWidth `div` 2 - 1)

-- | Pos addition
add :: Pos -> Pos -> Pos
(h1, w1) `add` (h2, w2) = (h1 + h2, w1 + w2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- * Task B4
-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape s && wellSize == wellSize'
  where
    (_, s)    = piece t
    wellSize' = shapeSize $ well t

-- * Task B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = Shape $ [outer] ++ inner ++ [outer]
  where
    rs    = rows s
    rSize = length (head rs) + 2 -- add additional 2 for walls
    outer = replicate rSize (Just Black)
    inner = map (\r -> [Just Black] ++ r ++ [Just Black]) rs

-- * Task B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris p w _) = addWalls $ combine (place p) w

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = map (\r -> allShapes !! getIdx r) rs
    where
      getIdx r = floor $ r * fromIntegral (length allShapes)

-- * Task B7
move :: (Int, Int) -> Tetris -> Tetris
move p (Tetris (p', s) w ss) = Tetris (p `add` p', s) w ss 

-- * Task B8
tick :: Tetris -> Maybe (Int, Tetris)
tick t
  | collision newState = dropNewPiece t 
  | otherwise          = Just (0, newState)
 where
    newState = move (1, 0) t

-- * Task C1: collision detection
collision :: Tetris -> Bool
collision (Tetris (p@(py, px), s) w _)
   | px < 0                    = True
   | px + sw > ww              = True
   | py + sh > wh              = True
   | place (p, s) `overlaps` w = True
   | otherwise                 = False
   where
      (sh,sw) = shapeSize s
      (wh,ww) = shapeSize w

-- * Task C3: movePiece function
movePiece :: Int -> Tetris -> Tetris
movePiece n t
  | collision newState = t
  | otherwise          = newState
  where
    newState = move (0, n) t

-- * Task C4: rotate function
rotate :: Tetris -> Tetris
rotate (Tetris (p, s) w ss) = Tetris (p, s') w ss
  where
    s' = rotateShape s

-- * Task C5: adjust function (optional)
-- (will be added if time permits)
adjust :: Tetris -> Tetris
adjust = undefined

-- * Task C6: handle rotate action
rotatePiece :: Tetris -> Tetris
rotatePiece t
  | collision newState = t
  | otherwise          = newState
  where
    newState = rotate t

-- * Task C7: piling up
dropNewPiece:: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris p w (s:ss)) 
  | place p' `overlaps` w' = Nothing
  | otherwise              = Just (n, t')
  where
    t'      = Tetris p' w' ss
    p'      = (startPosition, s)
    (n, w') = clearLines $ combine (place p) w

-- * Task C9: clearing rows
clearLines :: Shape -> (Int, Shape)
clearLines (Shape rs) = (n, s')
   where
      rs' = filter (not . all isJust) rs
      n   = length rs - length rs'
      s'  = shiftShape (n,0) (Shape rs')

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick      t = tick t
stepTetris MoveDown  t = tick t
stepTetris MoveLeft  t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris Rotate    t = Just (0, rotatePiece t) 
