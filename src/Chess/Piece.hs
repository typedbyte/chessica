{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Piece
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to create and analyze chess pieces.
-----------------------------------------------------------------------------
module Chess.Piece
  ( -- * Representing Pieces
    PieceType'(..)
  , PieceType(..)
  , Piece(..)
  , somePiece
  , fromSome
    -- * Analyzing Piece Types
  , equals
    -- * Analyzing Pieces
  , same
  , isOfType
  , assume
  )where

-- base
import Control.Applicative (Alternative, empty)
import Data.Type.Equality  ((:~:)(Refl), TestEquality, testEquality)

import Chess.Color (Color)
import Chess.Some  (Some(Some))

-- | Represents the piece types involved in a chess game on the type-level.
data PieceType'
  = Pawn'
  | Knight'
  | Bishop'
  | Rook'
  | Queen'
  | King'
  deriving (Eq, Ord, Read, Show)

-- | Represents the piece types involved in a chess game on the term-level.
data PieceType t where
  Pawn   :: PieceType Pawn'
  Knight :: PieceType Knight'
  Bishop :: PieceType Bishop'
  Rook   :: PieceType Rook'
  Queen  :: PieceType Queen'
  King   :: PieceType King'

deriving instance Eq   (PieceType t)
deriving instance Ord  (PieceType t)
deriving instance Show (PieceType t)

instance TestEquality PieceType where
  testEquality Pawn   Pawn   = Just Refl
  testEquality Knight Knight = Just Refl
  testEquality Bishop Bishop = Just Refl
  testEquality Rook   Rook   = Just Refl
  testEquality Queen  Queen  = Just Refl
  testEquality King   King   = Just Refl
  testEquality _      _      = Nothing

-- | Returns true if two piece types are the same.
equals :: PieceType a -> PieceType b -> Bool
equals type1 type2 =
  case testEquality type1 type2 of
    Just Refl -> True
    Nothing   -> False

-- | Represents a chess piece, which is a combination of its type and its color.
data Piece t = Piece
  { type' :: PieceType t
  , color :: Color
  }
  deriving (Eq, Ord, Show)

instance Eq (Some Piece) where
  Some p1 == Some p2 = same p1 p2

instance Ord (Some Piece) where
  compare (Some p1) (Some p2) =
    case (p1.type', p2.type') of
      (Pawn  , Pawn  ) -> compareColor
      (Pawn  , _     ) -> LT
      (Knight, Pawn  ) -> GT
      (Knight, Knight) -> compareColor
      (Knight, _     ) -> LT
      (Bishop, Pawn  ) -> GT
      (Bishop, Knight) -> GT
      (Bishop, Bishop) -> compareColor
      (Bishop, _     ) -> LT
      (Rook  , King  ) -> LT
      (Rook  , Queen ) -> LT
      (Rook  , Rook  ) -> compareColor
      (Rook  , _     ) -> GT
      (Queen , King  ) -> LT
      (Queen , Queen ) -> compareColor
      (Queen , _     ) -> GT
      (King  , King  ) -> compareColor
      (King  , _     ) -> GT
    where
      compareColor = compare p1.color p2.color

instance Show (Some Piece) where
  show (Some piece) = show piece

-- | Smart constructor for creating 'Some' 'Piece'.
somePiece :: PieceType t -> Color -> Some Piece
somePiece type' color = Some $ Piece type' color

-- | Smart constructor for creating 'Some' 'Piece' from 'Some' 'PieceType'.
fromSome :: Some PieceType -> Color -> Some Piece
fromSome (Some type') = somePiece type'

-- | Assumes that the given piece has the specified piece type.
assume :: Alternative f => PieceType t -> Piece a -> f (Piece t)
assume type' piece =
  case testEquality type' piece.type' of
    Just Refl -> pure piece
    Nothing   -> empty

-- | Returns true if two pieces are the same.
same :: Piece a -> Piece b -> Bool
same piece1 piece2 =
  case testEquality piece1.type' piece2.type' of
    Just Refl -> piece1.color == piece2.color
    Nothing   -> False

-- | Returns true if the given piece has the specified piece type.
isOfType :: PieceType t -> Piece a -> Bool
isOfType type' piece =
  case testEquality type' piece.type' of
    Just Refl -> True
    Nothing   -> False