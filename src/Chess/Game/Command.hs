{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Game.Command
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions for commands that can change chess game states.
-----------------------------------------------------------------------------
module Chess.Game.Command where

-- base
import GHC.Exts       (IsList(Item, fromList, toList))
import Prelude hiding (sequence)

import Chess.Board.PlacedPiece (PlacedPiece(..))
import Chess.Board.Position    (Position)
import Chess.Piece             (Piece)
import Chess.Some              (Some(Some))

-- | A command can be applied to a chess game state in order to obtain a new game state.
data Command
  = EndTurn
    -- ^ Ends the turn of the active player.
  | Move Position (Some PlacedPiece)
    -- ^ Moves a placed piece to the specified position.
  | Destroy (Some PlacedPiece)
    -- ^ Removes a placed piece.
  | Spawn Position (Some Piece)
    -- ^ Creates a chess piece on the specified position.
  | Promote (Some PlacedPiece) (Some Piece)
    -- ^ Converts a chess piece into another piece.
  | Sequence Command Command
    -- ^ Represents the consecutive execution of two commands.
  | Atomic Command
    -- ^ Denotes that a command and its sub-commands belong together and should
    -- be treated as single command (e.g., when recording the history of a chess game).
  deriving (Eq, Ord, Show)

instance IsList Command where
  type Item Command = Command
  
  fromList [cmd]    = cmd
  fromList (cmd:cs) = Sequence cmd (fromList cs)
  fromList []       = errorWithoutStackTrace "Command.fromList: empty list"
  
  toList (Sequence cmd1 cmd2) = cmd1 : toList cmd2
  toList cmd                  = [cmd]

-- | Smart constructor for 'EndTurn'.
endTurn :: Command
endTurn = EndTurn

-- | Smart constructor for 'Move'.
move :: Position -> PlacedPiece t -> Command
move position = Move position . Some

-- | Smart constructor for 'Destroy'.
destroy :: PlacedPiece t -> Command
destroy = Destroy . Some

-- | Smart constructor for 'Spawn'.
spawn :: Position -> Piece t -> Command
spawn position = Spawn position . Some

-- | Smart constructor for 'Promote'.
promote :: PlacedPiece a -> Piece b -> Command
promote placed piece = Promote (Some placed) (Some piece)

-- | Smart constructor for 'Sequence'.
sequence :: Command -> Command -> Command
sequence = Sequence

-- | Smart constructor for 'Atomic'.
atomic :: Command -> Command
atomic = Atomic

-- | Produces a command that has the opposite effect of the specified command.
undo :: Command -> Command
undo = \case
  EndTurn ->
    EndTurn
  Move dst (Some (PlacedPiece src piece)) ->
    move src (PlacedPiece dst piece)
  Destroy (Some placed) ->
    spawn placed.position placed.piece
  Spawn position (Some piece) ->
    destroy (PlacedPiece position piece)
  Promote (Some (PlacedPiece position old)) (Some new) ->
    promote (PlacedPiece position new) old
  Sequence cmd1 cmd2 ->
    sequence (undo cmd2) (undo cmd1)
  Atomic cmd ->
    atomic (undo cmd)