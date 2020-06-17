{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module    :  Data.Source.Range
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Range over a source file, this is different from a "Span" in that "Range"
-- only carries two indicies for an interval over a source.
--
--
-- @since 0.1.0.0

module Data.Source.Range
  ( Range(..), rangeLength
  ) where

import           Control.Lens
import           Data.Source.Span (HasSpanLike (..))
import           GHC.Generics

-- | An index range over a source file.
--
-- @since 0.1.0.0
data Range = Range
    { start :: {-# UNPACK #-} !Int -- ^ The start of the "Range".
    , end   :: {-# UNPACK #-} !Int -- ^ The end of the "Range".
    }
    deriving
      ( Eq      -- ^ @since 0.1.0.0
      , Generic -- ^ @since 0.1.0.0
      , Ord     -- ^ @since 0.1.0.0
      , Show    -- ^ @since 0.1.0.0
      )

-- | @since 0.1.0.0
instance Semigroup Range where
  Range s1 e1 <> Range s2 e2 = Range (min s1 s2) (max e1 e2)

-- | @since 0.1.0.0
instance HasSpanLike Range Int where
  start' = lens start (\s t -> s { start = t })
  {-# INLINE start' #-}

  end' = lens end (\s t -> s { end = t })
  {-# INLINE end' #-}

-- | Takes the length between beginning("start'") and end ("end'") of the range.
--
-- @since 0.1.0.0
rangeLength :: Range -> Int
rangeLength range = end range - start range
