{-# LANGUAGE NoImplicitPrelude, Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unicode.NormalizationInsensitive.Unsafe
-- Copyright   :  (c) 2011-2013 Bas van Dijk,
--                (c) 2016 Patrick Pelletier
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Patrick Pelletier <code@funwithsoftware.org>
--
-- Provides an unsafe way to create a normalization insensitive string-like value.
--
-----------------------------------------------------------------------------

module Data.Unicode.NormalizationInsensitive.Unsafe   ( unsafeMk ) where
import Data.Unicode.NormalizationInsensitive.Internal ( unsafeMk )
