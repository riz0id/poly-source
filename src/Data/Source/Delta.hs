
-- | Module    :  Parser.Token
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- File offset information.
--
-- @since 0.1.0.1

module Data.Source.Delta
  ( Delta(..)
  ) where

-- | Delta is a newtype wrapper over offset information in the a file input.
--
-- @since 0.1.0.0
data Delta = Delta { unDelta :: {-# UNPACK #-} !Int }
  deriving (Eq, Ord, Show)

-- | Additive monoid
--
-- @since 0.1.0.1
instance Semigroup Delta where
  Delta d1 <> Delta d2 = Delta (d1 + d2)

-- | @since 0.1.0.1
instance Monoid Delta where
  mempty = Delta 0
