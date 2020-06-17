{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Module    :  Data.Source.Loc
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Poly language tokens.
--
-- Location information for a source file.
--
-- @since 0.1.0.0

module Data.Source.Loc
  ( Loc, HasSpan(..), HasRange(..)
  ) where

import           Control.Lens
import           Data.Source.Range
import           Data.Source.Span
import           GHC.Generics
import           Prelude           hiding (span)

-- | Location information containing "span" and "range" over a file.
--
-- @since 0.1.0.0
data Loc = Loc
    { span  :: {-# UNPACK #-} !Span
    , range :: {-# UNPACK #-} !Range
    }
    deriving
      ( Eq      -- ^ @since 0.1.0.0
      , Generic -- ^ @since 0.1.0.0
      , Ord     -- ^ @since 0.1.0.0
      , Show    -- ^ @since 0.1.0.0
      )

-- | @since 0.1.0.0
instance Semigroup Loc where
  Loc s1 r1 <> Loc s2 r2 = Loc (s1 <> s2) (r1 <> r2)

-- | Classy-fields for record types which contain "Span"
--
-- @since 0.1.0.0
class HasSpan a where
  -- | "Span" lens
  --
  -- @since 0.1.0.0
  span' :: Lens' a Span

-- | @since 0.1.0.0
instance HasSpan Loc where
  span' = lens span (\s t -> s { span = t })

-- | Classy-fields for record types which contain "Range"
--
-- @since 0.1.0.0
class HasRange a where
  -- | "Range" lens
  --
  -- @since 0.1.0.0
  range' :: Lens' a Range

-- | @since 0.1.0.0
instance HasRange Loc where
  range' = lens range (\s t -> s { range = t })
