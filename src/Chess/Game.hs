{-# LANGUAGE OverloadedLists #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Game
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to create, manipulate and query chess game states.
-----------------------------------------------------------------------------
module Chess.Game
  ( -- * Representing Games
    Game(..)
  , Update(..)
    -- * Manipulating Games
  , execute
    -- * Querying Games
  , history
  , spawnCommands
    -- * Re-exports
  , module Chess.Game.Command
  , module Chess.Game.Status
  ) where

import Chess.Board             (Board, pieces, place, remove)
import Chess.Board.PlacedPiece (PlacedPiece(..))
import Chess.Exception         (ChessException)
import Chess.Game.Command
import Chess.Game.Status
import Chess.Player            (Player)
import Chess.Some              (Some(Some))

-- | Represents an update of a chess game state.
data Update = Update
  { game :: Game
    -- ^ Represents the chess game state before or after executing the corresponding command,
    -- depending on the context.
  , command :: Command
    -- ^ Represents the command that is involved in the game state update.
  }

-- | Represents a chess game state.
data Game = Game
  { board :: Board
    -- ^ The state of the chess board.
  , activePlayer :: Player
    -- ^ The player who is currently playing.
  , passivePlayer :: Player
    -- ^ The player who is currently waiting.
  , lastUpdate :: Maybe Update
    -- ^ The last update which led to this game state.
  }

-- | Executes a command on a game state in order to obtain a new game state.
execute :: Command -> Game -> Either ChessException Game
execute command game =
  case command of
    EndTurn ->
      pure
        game
          { activePlayer  = game.passivePlayer
          , passivePlayer = game.activePlayer
          }
    Move dst (Some placed) -> do
      tempBoard <- remove placed game.board
      newBoard <- place dst placed.piece tempBoard
      pure game { board = newBoard }
    Destroy (Some piece) -> do
      newBoard <- remove piece game.board
      pure game { board = newBoard }
    Spawn position (Some piece) -> do
      newBoard <- place position piece game.board
      pure game { board = newBoard }
    Sequence cmd1 cmd2 ->
      execute cmd1 game >>= execute cmd2
    Promote piece@(Some old) new ->
      execute [Destroy piece, Spawn old.position new] game
    Atomic cmd -> do
      newGame <- execute cmd game
      pure newGame { lastUpdate = Just (Update game command) }

-- | Gets the history of updates that led to the specified game state.
-- The most recent update is at the head of the list.
history :: Game -> [Update]
history game =
  case game.lastUpdate of
    Just update -> update : history update.game
    Nothing     -> []

-- | Gets a list of 'Spawn' commands which can be used to reconstruct
-- the board of the specified game state.
spawnCommands :: Game -> [Command]
spawnCommands game =
  flip fmap (pieces game.board) $ \(Some placedPiece) ->
    spawn placedPiece.position placedPiece.piece