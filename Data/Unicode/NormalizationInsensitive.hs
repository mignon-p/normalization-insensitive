{-# LANGUAGE CPP, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unicode.NormalizationInsensitive
-- Copyright   :  (c) 2011-2013 Bas van Dijk
--                (c) 2016 Patrick Pelletier
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Patrick Pelletier <code@funwithsoftware.org>
--
-- This module is intended to be imported qualified. May I suggest:
--
-- @
-- import           Data.Unicode.NormalizationInsensitive  ( NI )
-- import qualified Data.Unicode.NormalizationInsensitive as NI
-- @
--
-----------------------------------------------------------------------------

module Data.Unicode.NormalizationInsensitive ( NI
                            , mk
                            , original
                            , normalize
                            , map
                            , Normalizable(normalize)
                            ) where
import Data.Unicode.NormalizationInsensitive.Internal
