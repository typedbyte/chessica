-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Game.Status
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to handle the status of a chess game.
-----------------------------------------------------------------------------
module Chess.Game.Status where

import Chess.Player (Player)

-- | Represents the status of a chess game.
data Status
  = Win Player  -- ^ Indicates that the specified player has won.
  | Turn Player -- ^ Indicates that the specified player is currently playing.
  | Draw        -- ^ Indicates that the chess game has ended in a draw.