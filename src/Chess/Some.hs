-----------------------------------------------------------------------------
-- |
-- Module      : Chess.Some
-- Copyright   : (c) Michael Szvetits, 2023
-- License     : BSD-3-Clause (see the file LICENSE)
-- Maintainer  : typedbyte@qualified.name
-- Stability   : stable
-- Portability : portable
--
-- Types and functions to create an existential type.
-----------------------------------------------------------------------------
module Chess.Some where

-- | This existential type is used to "erase" the type parameter of a type, which
-- allows one to define types and functions that are agnostic to the erased type
-- parameter.
data Some f = forall t. Some (f t)