{-# LANGUAGE MultiWayIf #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook.Standard.Status
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Implementation of the game status, according to the standard rulebook.
-----------------------------------------------------------------------------
module Chess.Rulebook.Standard.Status where

-- base
import Data.Either (rights)

import Chess.Board                      (piecesOf)
import Chess.Game                       (Game(activePlayer, board, passivePlayer), execute)
import Chess.Game.Status                (Status(..))
import Chess.Player                     (Player(color))
import Chess.Rulebook.Standard.Check    (checked)
import Chess.Rulebook.Standard.Movement (movements)
import Chess.Some                       (Some(Some))

-- | Determines the status of a chess game, according to the standard rulebook.
status :: Game -> Status
status game =
  let
    activeColor   = game.activePlayer.color
    activePieces  = piecesOf activeColor game.board
    isChecked     = checked game.activePlayer game.board
    possibleMoves = concatMap (\(Some piece) -> movements piece game) activePieces
    futures       = rights $ fmap (flip execute game) possibleMoves
    hasMoves      = any (not . checked game.activePlayer . (.board)) futures
  in if
    | isChecked && not hasMoves     -> Win game.passivePlayer
    | not isChecked && not hasMoves -> Draw
    | otherwise                     -> Turn game.activePlayer