{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, Unsafe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unicode.NormalizationInsensitive.Internal
-- Copyright   :  (c) 2011-2013 Bas van Dijk
--                (c) 2016 Patrick Pelletier
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Patrick Pelletier <code@funwithsoftware.org>
--
-- Internal module which exports the 'NI' type, constructor,
-- associated instances and the 'Normalizable' class and instances.
--
-----------------------------------------------------------------------------

module Data.Unicode.NormalizationInsensitive.Internal ( NI
                                     , mk
                                     , unsafeMk
                                     , original
                                     , normalized
                                     , map
                                     , Normalizable(normalize)
                                     ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( (||) )
import Data.Eq       ( Eq, (==) )
import Data.Function ( on )
import Data.Monoid   ( Monoid, mempty, mappend )
import Data.Ord      ( Ord, compare )
import Data.String   ( IsString, fromString )
import Data.Data     ( Data )
import Data.Typeable ( Typeable )
import Prelude       ( String, (.), fmap, (&&), (+), (<=), otherwise )
import Text.Read     ( Read, readPrec )
import Text.Show     ( Show, showsPrec )

-- from bytestring:
import qualified Data.ByteString      as B  ( ByteString )
import qualified Data.ByteString.Lazy as BL ( ByteString, fromStrict, toStrict )

-- from text:
import qualified Data.Text      as T  ( Text, pack, unpack )
import qualified Data.Text.Lazy as TL ( Text, fromStrict, toStrict )

-- from deepseq:
import Control.DeepSeq ( NFData, rnf, deepseq )

-- from hashable:
import Data.Hashable ( Hashable, hashWithSalt )

-- from unicode-transforms:
import qualified Data.ByteString.UTF8.Normalize as B ( normalize )
import qualified Data.Text.Normalize            as T ( normalize )
import Data.Unicode.Types                       ( NormalizationMode(NFC) )

--------------------------------------------------------------------------------
-- Normalization Insensitive Strings
--------------------------------------------------------------------------------

{-| A @NI s@ provides /N/ormalization /I/nsensitive comparison for the string-like type
@s@ (for example: 'String', 'T.Text', 'B.ByteString', etc.).

Note that @NI s@ has an instance for 'IsString' which together with the
@OverloadedStrings@ language extension allows you to write normalization insensitive
string literals as in:

@
\> (\"Content-Type\" :: 'NI' 'T.Text') == (\"CONTENT-TYPE\" :: 'NI' 'T.Text')
True
@

-}
data NI s = NI { original   :: !s -- ^ Retrieve the original string-like value.
               , normalized :: !s -- ^ Retrieve the normalized string-like value.
                                  --   (Also see 'normalize').
               }
          deriving (Data, Typeable)

-- | Make the given string-like value normalization insensitive.
mk :: Normalizable s => s -> NI s
mk s = NI s (normalize s)

-- | Constructs a 'NI' from an already normalized string-like
-- value. The given string is used both as the 'original' as well as
-- the 'normalized'.
--
-- This function is unsafe since the compiler can't guarantee that the
-- provided string is normalized.
unsafeMk :: Normalizable s => s -> NI s
unsafeMk s = NI s s

-- | Transform the original string-like value but keep it normalized.
map :: Normalizable s2 => (s1 -> s2) -> (NI s1 -> NI s2)
map f = mk . f . original

instance (IsString s, Normalizable s) => IsString (NI s) where
    fromString = mk . fromString

instance (Monoid s, Normalizable s) => Monoid (NI s) where
    mempty = NI mempty mempty
    -- The result of concatenating two normalized strings is not
    -- necessarily normalized.  Therefore, concatenate the original
    -- strings and re-normalize.
    -- https://github.com/ppelleti/normalization-insensitive/issues/1
    NI o1 _ `mappend` NI o2 _ = NI o12 (normalize o12)
      where o12 = o1 `mappend` o2

instance Eq s => Eq (NI s) where
    (==) = (==) `on` normalized

instance Ord s => Ord (NI s) where
    compare = compare `on` normalized

instance (Read s, Normalizable s) => Read (NI s) where
    readPrec = fmap mk readPrec

instance Show s => Show (NI s) where
    showsPrec prec = showsPrec prec . original

instance Hashable s => Hashable (NI s) where
    hashWithSalt salt = hashWithSalt salt . normalized

instance NFData s => NFData (NI s) where
    rnf (NI o f) = o `deepseq` f `deepseq` ()

--------------------------------------------------------------------------------
-- Normalization
--------------------------------------------------------------------------------

mode = NFC

-- | Class of string-like types that support normalization.
class Normalizable s where
    normalize :: s -> s

-- | Note that @normalize@ on @'B.ByteString's@ assumes UTF-8 encoded strings!
instance Normalizable B.ByteString where
    normalize = B.normalize mode

-- | Note that @normalize@ on @'BL.ByteString's@ assumes UTF-8 encoded strings!
instance Normalizable BL.ByteString where
    normalize = BL.fromStrict . B.normalize mode . BL.toStrict

instance Normalizable String where
    normalize = T.unpack . T.normalize mode . T.pack

instance Normalizable T.Text where
    normalize = T.normalize mode

instance Normalizable TL.Text where
    normalize = TL.fromStrict . T.normalize mode . TL.toStrict

instance Normalizable (NI s) where
    normalize (NI _ l) = NI l l
