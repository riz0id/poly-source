{-# LANGUAGE DeriveGeneric #-}

-- | Positions in a source file.
--
-- @since 0.1.0.0

module Data.Source.Pos
  ( Pos, HasPos(..)
  ) where

import           Control.Lens
import           GHC.Generics

-- | Column and line information for a arbitrary source file.
--
-- @since 0.1.0.0
data Pos = Pos
    { line   :: {-# UNPACK #-} !Int
    , column :: {-# UNPACK #-} !Int
    }
    deriving
      ( Eq      -- ^ @since 0.1.0.0
      , Generic -- ^ @since 0.1.0.0
      , Ord     -- ^ @since 0.1.0.0
      , Show    -- ^ @since 0.1.0.0
      )

-- | Classy-fields for record types which contain positions.
--
-- @since 0.1.0.0
class HasPos a where
  -- | "Pos" lens
  --
  -- @since 0.1.0.0
  pos' :: Lens' a Pos

  -- | "Pos" lens for accessing the position's line.
  --
  -- @since 0.1.0.0
  line' :: Lens' a Int
  line' = pos' . line'

  -- | "Column" lens for accessing the position's column.
  --
  -- @since 0.1.0.0
  column' :: Lens' a Int
  column' = pos' . line'

-- | @since 0.1.0.0
instance HasPos Pos where
  pos' = id
