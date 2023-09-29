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

-- B4
-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_Shape s && wellSize == wellSize'
  where
    (_, s)    = piece t
    wellSize' = shapeSize $ well t

-- B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = Shape $ [outer] ++ inner ++ [outer]
  where
    rs    = rows s
    rSize = length (head rs) + 2 -- add additional 2 for walls
    outer = replicate rSize (Just Black)
    inner = map (\r -> [Just Black] ++ r ++ [Just Black]) rs

-- B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (p, s) w _) = addWalls $ combine (shiftShape p s) w

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well         = emptyShape wellSize
  piece:supply = repeat (allShapes !! 1) -- incomplete !!!

-- B7
move :: (Int, Int) -> Tetris -> Tetris
move p (Tetris (p', s) w ss) = Tetris (p `add` p', s) w ss 

-- B8
tick :: Tetris -> Maybe (Int, Tetris)
tick t = Just (0, move (1, 0) t) 

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick = tick
stepTetris _    = undefined
