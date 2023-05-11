{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Board.PlacedPiece
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to create and analyze placed chess pieces.
-----------------------------------------------------------------------------
module Chess.Board.PlacedPiece
  ( -- * Representing Placed Pieces
    PlacedPiece(..)
  , placedPiece
    -- * Analyzing Placed Pieces
  , assumeType
  ) where

-- base
import Control.Applicative (Alternative, empty)
import Data.Type.Equality  ((:~:)(Refl), testEquality)
import GHC.Records         (HasField, getField)

import Chess.Board.Position (Position)
import Chess.Color          (Color)
import Chess.Piece          (Piece(..), PieceType, same)
import Chess.Some           (Some(Some))

-- | Represents a chess piece that is currently placed on the board.
data PlacedPiece t = PlacedPiece
  { position :: Position
    -- ^ The position of the placed chess piece.
  , piece :: Piece t
    -- ^ The placed chess piece.
  }
  deriving (Eq, Ord, Show)

instance Eq (Some PlacedPiece) where
  Some p1 == Some p2 =
    p1.position == p2.position &&
    same p1.piece p2.piece

instance Ord (Some PlacedPiece) where
  compare (Some p1) (Some p2) =
    compare p1.position p2.position <>
    compare (Some p1.piece) (Some p2.piece)

instance Show (Some PlacedPiece) where
  show (Some piece) = show piece

instance HasField "color" (PlacedPiece t) Color where
  getField = (.piece.color)
  {-# INLINE getField #-}

instance HasField "type'" (PlacedPiece t) (PieceType t) where
  getField = (.piece.type')
  {-# INLINE getField #-}

-- | Smart constructor for creating 'Some' 'PlacedPiece'.
placedPiece :: Position -> Piece t -> Some PlacedPiece
placedPiece position = Some . PlacedPiece position

-- | Assumes that the given placed piece has the specified piece type.
assumeType :: Alternative f => PieceType t -> PlacedPiece a -> f (PlacedPiece t)
assumeType type' placed =
  case testEquality type' placed.piece.type' of
    Just Refl -> pure placed
    Nothing   -> empty