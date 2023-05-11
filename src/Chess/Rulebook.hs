-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Rulebook
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to define rulebooks for chess games.
-----------------------------------------------------------------------------
module Chess.Rulebook where

import Chess.Board.Position (Position)
import Chess.Game           (Game, Update)
import Chess.Game.Status    (Status)

-- | Represents a rulebook for a chess game.
data Rulebook = Rulebook
  { newGame :: Game
    -- ^ Creates a new chess game, according to the rulebook.
  , status :: Game -> Status
    -- ^ Gets the status of a chess game, according to the rulebook.
  , updates :: Position -> Game -> [Update]
    -- ^ Gets all possible updates (i.e., future game states) for a chess piece
    -- on a specified position, according to the rulebook.
  }