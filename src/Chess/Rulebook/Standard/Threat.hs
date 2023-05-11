{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Threat
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of chess piece threats, according to the standard rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Threat
  ( threats
  , threatCommands
  ) where

-- base
import Prelude hiding (lookup)

import Chess.Board             (Board, lookup)
import Chess.Board.Direction   (Direction(..), diagonals, jumps, orthogonals, principals)
import Chess.Board.PlacedPiece (PlacedPiece(..))
import Chess.Board.Position    (Position, offset)
import Chess.Color             (Color(White))
import Chess.Game.Command      (Command, move, destroy)
import Chess.Piece             (PieceType(..))
import Chess.Some              (Some(Some))

-- | Determines the positions threatened by a given chess piece.
threats :: PlacedPiece t -> Board -> [Position]
threats piece board =
  case piece.type' of
    Pawn   -> threat pawnDirections 1
    Knight -> threat jumps 1
    Bishop -> threat diagonals maxBound
    Rook   -> threat orthogonals maxBound
    Queen  -> threat principals maxBound
    King   -> threat principals 1
  where
    threat = reach piece board
    pawnDelta = if piece.color == White then 1 else (-1)
    pawnDirections = filter (\d -> d.rowDelta == pawnDelta) diagonals

-- | Determines all possible threat-based movements (including captures) for a
-- given chess piece.
threatCommands :: PlacedPiece t -> Board -> [Command]
threatCommands piece board = do
  target <- threats piece board
  case lookup target board of
    Nothing ->
      pure (move target piece)
    Just (Some enemy) ->
      pure [destroy enemy, move target piece]

-- | Starting at a specified position, provides a list of all the positions
--   that are reachable by a chess piece in the given directions.
reach
  :: PlacedPiece t -- ^ The analyzed chess piece.
  -> Board         -- ^ The current chess board state.
  -> [Direction]   -- ^ The directions to iterate, beginning by the starting position.
  -> Int           -- ^ The maximum count of steps to take in a specific direction.
  -> [Position]    -- ^ A list of all the reachable positions.
reach piece board directions maxSteps = do
  direction <- directions
  takeUntilPiece
    $ take maxSteps
    $ collect (offset direction) piece.position
  where
    -- takeUntilPiece iterates until a friend or enemy piece is hit
    takeUntilPiece [] = []
    takeUntilPiece (p:ps) =
      case lookup p board of
        Nothing -> p : takeUntilPiece ps
        Just (Some other)
          | piece.color == other.color -> []
          | otherwise                  -> [p]
    -- collect is catMaybes, but only until the first Nothing
    collect f start = go (f start)
      where
        go (Just p) = p : go (f p)
        go Nothing  = []