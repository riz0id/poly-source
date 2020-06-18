{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Module    :  Data.Source
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

module Data.Source
  ( -- * Sources
    Source(..), length, null, totalRange
    -- ** Source Construction
  , fromUTF8
    -- ** Slicing Sources
  , slice, take, drop
    -- * Locations
  , Loc
    -- ** Location Classes
  , HasSpan(..), HasRange(..)
    -- * Positions
  , Pos, emptyPos
    -- ** Position Classes
  , HasPos(..)
    -- * Ranges
  , Range(..), rangeLength
    -- * Spans
  , Span(..)
    -- ** Span Classes
  , HasSpanLike(..)
    -- * Deltas
  , Delta(..)
    -- ** Delta Classes
  , HasDelta(..)
  ) where

import qualified Data.ByteString   as B
import           Data.Source.Delta
import           Data.Source.Loc
import           Data.Source.Pos
import           Data.Source.Range
import           Data.Source.Span
import           Data.String       (IsString (..))
import           GHC.Generics
import           Prelude           hiding (drop, length, null, take)

-- | A source file viewed as a wrapper over a "ByteString"
--
-- @since 0.1.0.0
newtype Source = Source { bytes :: B.ByteString }
  deriving (Eq, Semigroup, Monoid, IsString, Show, Generic)

-- | Construct a "Source" from a UTF8 "ByteString".
--
-- @since 0.1.0.0
fromUTF8 :: B.ByteString -> Source
fromUTF8 = Source

-- | Retrieves the length of the "Source".
--
-- @since 0.1.0.0
length :: Source -> Int
length = B.length . bytes

-- | Asks if the source is a empty "ByteString"
--
-- @since 0.1.0.0
null :: Source -> Bool
null = B.null . bytes

-- | Gets the length of a "Source" as a "Range".
--
-- @since 0.1.0.0
totalRange :: Source -> Range
totalRange = Range 0 . Delta . B.length . bytes

-- | Slices the "Source" provided by a given "Range".
--
-- @since 0.1.0.0
slice :: Source -> Range -> Source
slice source range = taking $ dropping source where
  dropping = drop (unDelta (rangeStart range))
  taking   = take (unDelta (rangeLength range))

-- | Drops n bytes from the end of a "Source".
--
-- @since 0.1.0.0
drop :: Int -> Source -> Source
drop i = Source . B.drop i . bytes

-- | Takes away n bytes from the beginning of a "Source".
--
-- @since 0.1.0.0
take :: Int -> Source -> Source
take i = Source . B.take i . bytes
