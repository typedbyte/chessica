{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Exception
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for exceptions that can occur when manipulating chess
-- games.
-----------------------------------------------------------------------------
module Chess.Exception where

-- base
import Control.Applicative (Alternative, empty)

import Chess.Board.PlacedPiece (PlacedPiece)
import Chess.Some              (Some(Some))

-- | Represents errors that can occur when manipulating chess games.
data ChessException
  = FieldOccupied (Some PlacedPiece)
  | PieceMissing (Some PlacedPiece)
  | UnexpectedPiece (Some PlacedPiece)

-- | Smart constructor for 'FieldOccupied'.
fieldOccupied :: PlacedPiece t -> ChessException
fieldOccupied = FieldOccupied . Some

-- | Smart constructor for 'PieceMissing'.
pieceMissing :: PlacedPiece t -> ChessException
pieceMissing = PieceMissing . Some

-- | Smart constructor for 'UnexpectedPiece'.
unexpectedPiece :: PlacedPiece t -> ChessException
unexpectedPiece = UnexpectedPiece . Some

-- | Extracts the 'Right' value from an 'Either'.
assumeRight :: Alternative f => Either e a -> f a
assumeRight = \case
  Right a -> pure a
  Left _  -> empty