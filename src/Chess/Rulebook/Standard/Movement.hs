{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Movement
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the movement rules, according to the standard rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Movement where

import Chess.Board.PlacedPiece                 (PlacedPiece)
import Chess.Game                              (Game(Game, board))
import Chess.Game.Command                      (Command)
import Chess.Piece                             (PieceType(..))
import Chess.Rulebook.Standard.Movement.Bishop qualified as Bishop
import Chess.Rulebook.Standard.Movement.King   qualified as King
import Chess.Rulebook.Standard.Movement.Knight qualified as Knight
import Chess.Rulebook.Standard.Movement.Pawn   qualified as Pawn
import Chess.Rulebook.Standard.Movement.Queen  qualified as Queen
import Chess.Rulebook.Standard.Movement.Rook   qualified as Rook

-- | Determines all possible movements (including captures and promotions) for a given chess piece.
movements :: PlacedPiece t -> Game -> [Command]
movements piece game@Game{board} =
  case piece.type' of
    Pawn   -> Pawn.movements piece game
    Knight -> Knight.movements piece board
    Bishop -> Bishop.movements piece board
    Rook   -> Rook.movements piece board
    Queen  -> Queen.movements piece board
    King   -> King.movements piece game