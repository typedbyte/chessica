-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Player
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to handle chess players.
-----------------------------------------------------------------------------
module Chess.Player where

import Chess.Color (Color)

-- | Represents a player who participates in a chess game.
--
-- Currently, a player is only identified by the color of the controlled chess pieces.
-- In the future, a player could have more attributes, like name, ELO rating, etc.
newtype Player = Player { color :: Color }
  deriving (Eq, Ord, Read, Show)