-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Board
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to create, manipulate and query chess boards.
-----------------------------------------------------------------------------
module Chess.Board
  ( -- * Representing Boards
    Board
  , empty
    -- * Manipulating Boards
  , place
  , replace
  , remove
    -- * Querying Boards
  , lookup
  , isOccupied
  , pieces
  , piecesOf
  -- * Re-exports
  , module Chess.Board.Direction
  , module Chess.Board.PlacedPiece
  , module Chess.Board.Position
  ) where

-- base
import Control.Applicative qualified as A
import Data.Maybe          (isJust)
import Prelude      hiding (lookup)

-- containers
import Data.Map.Strict qualified as M

import Chess.Board.Direction
import Chess.Board.PlacedPiece
import Chess.Board.Position
import Chess.Color             (Color)
import Chess.Exception         (ChessException, fieldOccupied, pieceMissing, unexpectedPiece)
import Chess.Piece             (Piece(color), same)
import Chess.Some              (Some(Some))

-- | Represents a chess board.
newtype Board = Board { pieces :: M.Map Position (Some Piece) }

-- | An empty chess board, i.e. a board with no placed pieces.
empty :: Board
empty = Board M.empty

-- | Introduces a new chess piece to the chess board at a specific position.
place
  :: Position
  -- ^ The position of the newly introduced chess piece.
  -> Piece t
  -- ^ The newly introduced chess piece.
  -> Board
  -- ^ The original chess board.
  -> Either ChessException Board
  -- ^ The new chess board, if the specified position has not been occupied.
place position piece board =
  let
    (maybeOldPiece, newPieces) =
      M.insertLookupWithKey
        ( \_key new _old -> new )
        ( position )
        ( Some piece )
        ( board.pieces )
  in
    case maybeOldPiece of
      Just (Some oldPiece) ->
        Left $ fieldOccupied (PlacedPiece position oldPiece)
      Nothing ->
        Right $ Board newPieces

-- | Introduces a new chess piece to the chess board at a specific position.
replace
  :: Position
  -- ^ The position of the newly introduced chess piece.
  -> Piece t
  -- ^ The newly introduced chess piece.
  -> Board
  -- ^ The original chess board.
  -> Board
  -- ^ The new chess board. If the specified position has been occupied, the piece is replaced.
replace position piece (Board oldPieces) =
  Board (M.insert position (Some piece) oldPieces)

-- | Removes a chess piece from the chess board at a given position.
remove
  :: PlacedPiece t
  -- ^ The position of the chess piece to be removed.
  -> Board
  -- ^ The original chess board.
  -> Either ChessException Board
  -- ^ The new chess board, if the specified position was indeed occupied by the piece.
remove placed@PlacedPiece{position, piece} board =
  let
    (maybeOldPiece, newPieces) =
      M.updateLookupWithKey
        ( \_ _ -> Nothing )
        ( position )
        ( board.pieces )
  in
    case maybeOldPiece of
      Nothing -> Left $ pieceMissing placed
      Just (Some oldPiece)
        | same oldPiece piece -> Right $ Board newPieces
        | otherwise           -> Left $ unexpectedPiece (PlacedPiece position oldPiece)

-- | Gets a chess piece at a specific position of the chess board.
lookup
  :: A.Alternative f
  => Position
  -- ^ The position to look for a chess piece.
  -> Board
  -- ^ The board used for the lookup.
  -> f (Some PlacedPiece)
  -- ^ The chess piece, if the specified position was indeed occupied by it.
lookup position board =
  case M.lookup position board.pieces of
    Just (Some piece) -> pure $ placedPiece position piece
    Nothing           -> A.empty

-- | Checks if a specified position of the chess board is occupied by a chess piece.
isOccupied
  :: Position -- ^ The position to be checked for occupation.
  -> Board    -- ^ The board whose position is checked for occupation.
  -> Bool     -- ^ True if the specified position is occupied, or else false.
isOccupied position =
  isJust . lookup position

-- | Gets all chess pieces that are currently on the chess board.
pieces :: Board -> [Some PlacedPiece]
pieces
  = fmap (\(position, Some piece) -> placedPiece position piece)
  . M.assocs
  . (.pieces)

-- | Gets all chess pieces of a given color that are currently on the chess board.
piecesOf :: Color -> Board -> [Some PlacedPiece]
piecesOf color
  = fmap (\(position, Some piece) -> placedPiece position piece)
  . filter (\(_, Some piece) -> piece.color == color)
  . M.assocs
  . (.pieces)
