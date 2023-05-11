{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Movement.Queen
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the movement rule for queens, according to the standard
-- rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Movement.Queen where

import Chess.Board                    (Board)
import Chess.Board.PlacedPiece        (PlacedPiece)
import Chess.Game.Command             (Command)
import Chess.Piece                    (PieceType'(Queen'))
import Chess.Rulebook.Standard.Threat (threatCommands)

-- | Determines all possible movements (including captures) for a given queen.
movements :: PlacedPiece Queen' -> Board -> [Command]
movements = threatCommands