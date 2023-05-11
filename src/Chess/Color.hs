-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Color
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to handle the colors of a chess game.
-----------------------------------------------------------------------------
module Chess.Color where

-- | The colors involved in a chess game.
data Color = White | Black
  deriving (Eq, Ord, Read, Show)

-- | Gets the opposite color of a specified color.
oppositeOf :: Color -> Color
oppositeOf White = Black
oppositeOf Black = White