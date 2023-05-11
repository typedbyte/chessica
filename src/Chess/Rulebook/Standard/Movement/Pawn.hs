{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Movement.Pawn
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the movement rule for pawns, according to the standard
-- rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Movement.Pawn
  ( movements
  , oneStep
  , twoSteps
  , capture
  , enPassant
  ) where

-- base
import Control.Applicative ((<|>))
import Control.Monad       (guard)
import Data.Maybe          (maybeToList)
import Prelude hiding      (lookup)

import Chess.Board                    (Board, isOccupied, lookup)
import Chess.Board.Direction          (Direction, up, down)
import Chess.Board.PlacedPiece        (PlacedPiece(..))
import Chess.Board.Position           (Position(..), boundedPosition, offset)
import Chess.Color                    (Color(..))
import Chess.Game                     (Game(..), Update(command))
import Chess.Game.Command             (Command(..), move, promote, destroy)
import Chess.Piece                    (Piece(Piece), PieceType(..), PieceType'(Pawn'), isOfType)
import Chess.Rulebook.Standard.Threat (threats)
import Chess.Some                     (Some(Some))

-- | Determines all possible movements (including captures, promotions and en
-- passant) for a given pawn.
movements :: PlacedPiece Pawn' -> Game -> [Command]
movements pawn game@Game{board}
    = oneStep pawn board
  <|> twoSteps pawn board
  <|> capture pawn board
  <|> maybeToList (enPassant pawn game)

-- | Determines all possible one-step forward movements (including promotions)
-- for a given pawn.
oneStep :: PlacedPiece Pawn' -> Board -> [Command]
oneStep pawn board = do
  let pawnDirection = direction pawn
  forward <- offset pawnDirection pawn.position
  guard $ not $ isOccupied forward board
  maybePromote forward pawn (move forward pawn)

-- | Determines all possible two-step forward movements for a given pawn.
twoSteps :: PlacedPiece Pawn' -> Board -> [Command]
twoSteps pawn board = do
  guard $
    case pawn.color of
      White -> pawn.position.row == 1
      Black -> pawn.position.row == 6
  let pawnDirection = direction pawn
  oneForward <- offset pawnDirection pawn.position
  guard $ not $ isOccupied oneForward board
  twoForward <- offset pawnDirection oneForward
  guard $ not $ isOccupied twoForward board
  pure (move twoForward pawn)

-- | Determines all possible capture movements (including promotions, excluding
-- en passant) for a given pawn.
capture :: PlacedPiece Pawn' -> Board -> [Command]
capture pawn board = do
  target     <- threats pawn board
  Some enemy <- lookup target board
  maybePromote target pawn [destroy enemy, move target pawn]

-- | Determines the en passant movement for a given pawn, if possible.
enPassant :: PlacedPiece Pawn' -> Game -> Maybe Command
enPassant pawn game =
  let
    expectedRow =
      case pawn.color of
        White -> 4
        Black -> 3
    lastMove = \case
      Move dst (Some (PlacedPiece src movedPiece)) ->
        Just (src, dst, Some movedPiece)
      Sequence cmd _ ->
        lastMove cmd
      Atomic cmd ->
        lastMove cmd
      _ ->
        Nothing
  in do
    update <- game.lastUpdate
    (src, dst, Some enemy) <- lastMove update.command
    guard
      -- the pawn must be in the correct row
       $ pawn.position.row == expectedRow
      -- the previous move of the enemy must include a pawn
      && isOfType Pawn enemy
      -- the enemy's pawn must have ended its move next to the pawn
      && pawn.position.row == dst.row
      && abs (pawn.position.column - dst.column) == 1
      -- the enemy's pawn must have moved two fields
      && abs (src.row - dst.row) == 2
    let
      newPosition =
        boundedPosition
          ( (src.row + dst.row) `div` 2 )
          ( dst.column )
    pure [destroy (PlacedPiece dst enemy), move newPosition pawn]

promotions :: PlacedPiece Pawn' -> [Command]
promotions pawn = do
  guard $
    ( pawn.color == White && pawn.position.row == 7 ) ||
    ( pawn.color == Black && pawn.position.row == 0 )
  [  promote pawn (Piece Queen  pawn.color)
   , promote pawn (Piece Rook   pawn.color)
   , promote pawn (Piece Bishop pawn.color)
   , promote pawn (Piece Knight pawn.color) ]

direction :: PlacedPiece Pawn' -> Direction
direction pawn =
  case pawn.color of
    White -> up
    Black -> down

maybePromote :: Position -> PlacedPiece Pawn' -> Command -> [Command]
maybePromote position pawn moveCmd =
  case promotions (pawn {position = position}) of
    [] -> [moveCmd]
    ps -> [[moveCmd, p] | p <- ps]