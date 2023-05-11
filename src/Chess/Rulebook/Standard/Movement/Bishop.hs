{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Movement.Bishop
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the movement rule for bishops, according to the standard
-- rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Movement.Bishop where

import Chess.Board                    (Board)
import Chess.Board.PlacedPiece        (PlacedPiece)
import Chess.Game.Command             (Command)
import Chess.Piece                    (PieceType'(Bishop'))
import Chess.Rulebook.Standard.Threat (threatCommands)

-- | Determines all possible movements (including captures) for a given bishop.
movements :: PlacedPiece Bishop' -> Board -> [Command]
movements = threatCommands