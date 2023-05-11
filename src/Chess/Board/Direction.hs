-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Board.Direction
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to represent and manipulate directions on a chess board.
-----------------------------------------------------------------------------
module Chess.Board.Direction where

-- | Represents a two-dimensional direction on a chess board.
data Direction = Direction
  { rowDelta :: Int
    -- ^ The row component of the direction.
  , columnDelta :: Int
    -- ^ The column component of the direction.
  }

-- | Yields the integral vector pointing to the left, where the leftmost column is labelled A.
left :: Direction
left = Direction 0 (-1)

-- | Yields the integral vector pointing to the right, where the leftmost column is labelled A.
right :: Direction
right = Direction 0 1

-- | Yields the integral vector pointing up, where the lowest row is labelled 1.
up :: Direction
up = Direction 1 0

-- | Yields the integral vector pointing down, where the lowest row is labelled 1.
down :: Direction
down = Direction (-1) 0

-- | Yields the four integral vectors in the directions up, right, down and left.
orthogonals :: [Direction]
orthogonals = [up, right, down, left]

-- | Yields the four integral vectors in the directions left up, right up, right down and left down.
diagonals :: [Direction]
diagonals =
  [ Direction   1  (-1)
  , Direction   1    1
  , Direction (-1)   1
  , Direction (-1) (-1)
  ]

-- | Yields the combination of orthogonal and diagonal integral vectors.
principals :: [Direction]
principals = orthogonals ++ diagonals

-- | Yields the eight directions a knight is able to jump, according to the standard rules.
jumps :: [Direction]
jumps =
  [ Direction row column
    | row    <- [-1, 1, 2, -2]
    , column <- [-1, 1, 2, -2]
    , row /= column
    , row /= -column
  ]