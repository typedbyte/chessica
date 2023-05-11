{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to represent the standard rulebook for chess games.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard (standardRulebook) where

-- base
import Control.Monad  (guard)
import Prelude hiding (lookup)

import Chess.Board                      (Board, empty, lookup, replace)
import Chess.Board.PlacedPiece          (PlacedPiece(PlacedPiece), placedPiece)
import Chess.Board.Position             (boundedPosition)
import Chess.Color                      (Color(..))
import Chess.Exception                  (assumeRight)
import Chess.Game                       (Game(..), Update(..), execute)
import Chess.Game.Command               (atomic, endTurn)
import Chess.Piece                      (Piece(Piece), PieceType(..))
import Chess.Player                     (Player(Player))
import Chess.Rulebook                   (Rulebook(..))
import Chess.Rulebook.Standard.Check    (checked)
import Chess.Rulebook.Standard.Movement (movements)
import Chess.Rulebook.Standard.Status   (status)
import Chess.Some                       (Some(Some))

-- | The standard rulebook for chess games.
standardRulebook :: Rulebook
standardRulebook =
  Rulebook
  { newGame =
      Game
        { board         = newBoard
        , activePlayer  = Player White
        , passivePlayer = Player Black
        , lastUpdate    = Nothing
        }
  , status = status
  , updates =
      \position game -> do
        Some piece <- lookup position game.board
        move <- movements piece game
        let turn = atomic [move, endTurn]
        newGame <- assumeRight $ execute turn game
        guard $ not (checked newGame.passivePlayer newGame.board)
        pure (Update newGame turn)
  }

baseLine :: Color -> [Some PlacedPiece]
baseLine color =
  let
    rook   = Piece Rook color
    knight = Piece Knight color
    bishop = Piece Bishop color
    queen  = Piece Queen color
    king   = Piece King color
    row    = if color == White then 0 else 7
  in
    [ placedPiece (boundedPosition row 0) rook
    , placedPiece (boundedPosition row 1) knight
    , placedPiece (boundedPosition row 2) bishop
    , placedPiece (boundedPosition row 3) queen
    , placedPiece (boundedPosition row 4) king
    , placedPiece (boundedPosition row 5) bishop
    , placedPiece (boundedPosition row 6) knight
    , placedPiece (boundedPosition row 7) rook
    ]

pawnLine :: Color -> [Some PlacedPiece]
pawnLine color =
  let
    pawn = Piece Pawn color
    row  = if color == White then 1 else 6
  in
    fmap
      ( \column -> placedPiece (boundedPosition row column) pawn )
      [ 0..7 ]

newBoard :: Board
newBoard =
  let
    whiteBaseLine = baseLine White
    whitePawnLine = pawnLine White
    blackBaseLine = baseLine Black
    blackPawnLine = pawnLine Black
  in
    foldr
      ( \(Some (PlacedPiece position piece)) -> replace position piece )
      ( empty )
      ( whiteBaseLine ++ whitePawnLine ++ blackBaseLine ++ blackPawnLine )