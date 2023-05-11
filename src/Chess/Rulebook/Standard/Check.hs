-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Check
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the check rule, according to the standard rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Check where

-- base
import Data.List (find)

import Chess.Board                    (Board, piecesOf)
import Chess.Board.PlacedPiece        (PlacedPiece(position, piece))
import Chess.Color                    (oppositeOf)
import Chess.Piece                    (PieceType(King), isOfType)
import Chess.Player                   (Player(color))
import Chess.Rulebook.Standard.Threat (threats)
import Chess.Some                     (Some(Some))

-- | Returns true if the specified player is currently checked, according to the
-- standard rulebook.
checked :: Player -> Board -> Bool
checked player board =
  let
    enemyThreats =
      concatMap
        ( \(Some enemy) -> threats enemy board )
        ( piecesOf (oppositeOf player.color) board )
    playerKing =
      find
        ( \(Some friend) -> isOfType King friend.piece )
        ( piecesOf player.color board )
  in
    case playerKing of
      Just (Some king) -> elem king.position enemyThreats
      Nothing          -> False