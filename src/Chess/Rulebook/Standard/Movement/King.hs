{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Movement.King
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the movement rule for kings, according to the standard
-- rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Movement.King where

-- base
import Control.Applicative ((<|>))
import Control.Monad       (guard)
import Prelude hiding      (lookup)

import Chess.Board                    (isOccupied, lookup, piecesOf)
import Chess.Board.Direction          (left, right)
import Chess.Board.PlacedPiece        (PlacedPiece(..), assumeType)
import Chess.Board.Position           (Position(..), boundedOffset, boundedPosition)
import Chess.Color                    (Color(..), oppositeOf)
import Chess.Game                     (Game(Game, board), Update(command), history)
import Chess.Game.Command             (Command(..), move)
import Chess.Piece                    (PieceType(Rook), PieceType'(King'))
import Chess.Rulebook.Standard.Threat (threats, threatCommands)
import Chess.Some                     (Some(Some))

-- | Determines all possible movements (including captures and castlings) for a given king.
movements :: PlacedPiece King' -> Game -> [Command]
movements king game@Game{board}
    = threatCommands king board
  <|> castlings king game

-- | Determines all possible castlings for a given king.
castlings :: PlacedPiece King' -> Game -> [Command]
castlings king game@Game{board} =
  let
    expectedPosition =
      case king.color of
        White -> boundedPosition 0 4
        Black -> boundedPosition 7 4
    enemyThreats =
      concatMap
        ( \(Some enemy) -> threats enemy board )
        ( piecesOf (oppositeOf king.color) board )
    hasMoved rookPosition = \case
      Move _ (Some (PlacedPiece src _)) ->
        src == king.position || src == rookPosition
      Sequence cmd1 cmd2 ->
        hasMoved rookPosition cmd1 || hasMoved rookPosition cmd2
      Atomic cmd ->
        hasMoved rookPosition cmd
      _ ->
        False
  in do
    -- the king must be in the right position
    guard $ king.position == expectedPosition
    -- the same-colored rook must be in the right position
    let leftCorner  = boundedPosition king.position.row 0
    let rightCorner = boundedPosition king.position.row 7
    Some piece <- lookup leftCorner board <|> lookup rightCorner board
    rook       <- assumeType Rook piece
    guard $ rook.color == king.color
    -- get the fields between king and rook
    let direction = if king.position.column > rook.position.column then left else right
    let oneNext   = boundedOffset direction king.position
    let twoNext   = boundedOffset direction oneNext
    guard
      -- the fields between king and rook must be empty
       $ not (isOccupied oneNext board)
      && not (isOccupied twoNext board)
      -- the king must not be threatened
      && not (elem king.position enemyThreats)
      -- the fields between king and rook must not be threatened
      && not (elem oneNext enemyThreats)
      && not (elem twoNext enemyThreats)
      -- king and rook must not have moved during the game
      && not
           ( any
             ( hasMoved rook.position . (.command) )
             ( history game )
           )
    pure [move twoNext king, move oneNext rook]