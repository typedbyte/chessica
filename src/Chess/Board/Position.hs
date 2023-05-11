-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Board.Position
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to represent and manipulate positions on a chess board.
-----------------------------------------------------------------------------
module Chess.Board.Position
  ( -- * Representing Positions
    Position(row, column)
  , mkPosition
  , boundedPosition
    -- * Manipulating Positions
  , offset
  , boundedOffset
  ) where

-- base
import Control.Applicative (Alternative, empty)

import Chess.Board.Direction (Direction(rowDelta, columnDelta))

-- | Represents a position on the chess board.
data Position = Position
  { row :: Int
    -- ^ The row of the position, where 0 is row 1.
  , column :: Int
    -- ^ The column of the position, where 0 is column A.
  }
  deriving (Eq, Ord, Read, Show)

-- | Creates a position from row and column indices.
mkPosition
  :: Alternative f
  => Int        -- ^ The row of the position, where 0 is the row labelled 1.
  -> Int        -- ^ The column of the position, where 0 is the column labelled A.
  -> f Position -- ^ The position, if it is within the bounds of the chess board.
mkPosition row column
  | between 0 7 row && between 0 7 column =
      pure $ Position row column
  | otherwise =
      empty
  where
    between low high value =
      low <= value && value <= high

-- | Creates a position from row and column indices.
boundedPosition
  :: Int      -- ^ The row of the position, where 0 is the row labelled 1.
  -> Int      -- ^ The column of the position, where 0 is the column labelled A.
  -> Position -- ^ The position, where out-of-bounds indices are limited to the valid range.
boundedPosition row column =
  Position
    ( clamp 0 7 row )
    ( clamp 0 7 column)
  where
    clamp lower upper value
      | value < lower = lower
      | value > upper = upper
      | otherwise     = value

-- | Adds an offset to a position, yielding a new position.
offset
  :: Alternative f
  => Direction  -- ^ The offset to be added to the position.
  -> Position   -- ^ The original position.
  -> f Position -- ^ The new position, if it is within the bounds of the chess board.
offset direction position =
  mkPosition
    ( position.row + direction.rowDelta )
    ( position.column + direction.columnDelta )

-- | Adds an offset to a position, yielding a new position.
boundedOffset
  :: Direction -- ^ The offset to be added to the position.
  -> Position  -- ^ The original position.
  -> Position  -- ^ The new position, where out-of-bounds indices are limited to the valid range.
boundedOffset direction position =
  boundedPosition
    ( position.row + direction.rowDelta )
    ( position.column + direction.columnDelta )