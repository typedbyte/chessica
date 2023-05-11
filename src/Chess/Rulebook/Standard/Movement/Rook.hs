{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Movement.Rook
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the movement rule for rooks, according to the standard
-- rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Movement.Rook where

import Chess.Board                    (Board)
import Chess.Board.PlacedPiece        (PlacedPiece)
import Chess.Game.Command             (Command)
import Chess.Piece                    (PieceType'(Rook'))
import Chess.Rulebook.Standard.Threat (threatCommands)

-- | Determines all possible movements (including captures) for a given rook.
movements :: PlacedPiece Rook' -> Board -> [Command]
movements = threatCommands