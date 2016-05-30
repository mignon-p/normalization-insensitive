{-# LANGUAGE CPP, DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Unicode.NormalizationInsensitive.Internal
-- Copyright   :  (c) 2011-2013 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
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
import Data.Char     ( Char, toLower )
import Data.Eq       ( Eq, (==) )
import Data.Function ( on )
import Data.Monoid   ( Monoid, mempty, mappend )
import Data.Ord      ( Ord, compare )
import Data.String   ( IsString, fromString )
import Data.Data     ( Data )
import Data.Typeable ( Typeable )
import Data.Word     ( Word8 )
import Prelude       ( (.), fmap, (&&), (+), (<=), otherwise )
import Text.Read     ( Read, readPrec )
import Text.Show     ( Show, showsPrec )

import qualified Data.List as L ( map )

#if __GLASGOW_HASKELL__ < 700
import Control.Monad ( (>>) )
import Prelude       ( fromInteger )
#endif

-- from bytestring:
import qualified Data.ByteString      as B  ( ByteString, map )
import qualified Data.ByteString.Lazy as BL ( ByteString, map )

-- from text:
import qualified Data.Text      as T  ( Text, toCaseFold )
import qualified Data.Text.Lazy as TL ( Text, toCaseFold, pack, unpack )

-- from deepseq:
import Control.DeepSeq ( NFData, rnf, deepseq )

-- from hashable:
import Data.Hashable ( Hashable, hashWithSalt )


--------------------------------------------------------------------------------
-- Normalization Insensitive Strings
--------------------------------------------------------------------------------

{-| A @NI s@ provides /C/ase /I/nsensitive comparison for the string-like type
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

instance Monoid s => Monoid (NI s) where
    mempty = NI mempty mempty
    NI o1 l1 `mappend` NI o2 l2 = NI (o1 `mappend` o2) (l1 `mappend` l2)

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

-- | Class of string-like types that support normalization.
class Normalizable s where
    normalize :: s -> s

    normalizeList :: [s] -> [s]
    normalizeList = L.map normalize

instance Normalizable a => Normalizable [a] where
    normalize = normalizeList

-- | Note that @normalize@ on @'B.ByteString's@ is only guaranteed to be correct for ISO-8859-1 encoded strings!
instance Normalizable B.ByteString where normalize = B.map toLower8

-- | Note that @normalize@ on @'BL.ByteString's@ is only guaranteed to be correct for ISO-8859-1 encoded strings!
instance Normalizable BL.ByteString where normalize = BL.map toLower8

instance Normalizable Char where
    normalize     = toLower
    normalizeList = TL.unpack . TL.toCaseFold . TL.pack

instance Normalizable T.Text  where normalize = T.toCaseFold
instance Normalizable TL.Text where normalize = TL.toCaseFold
instance Normalizable (NI s)  where normalize (NI _ l) = NI l l

{-# INLINE toLower8 #-}
toLower8 :: Word8 -> Word8
toLower8 w
  |  65 <= w && w <=  90 ||
    192 <= w && w <= 214 ||
    216 <= w && w <= 222 = w + 32
  | otherwise            = w

--------------------------------------------------------------------------------
-- Rewrite RULES
--------------------------------------------------------------------------------

{-# RULES "normalize/ByteString" normalize = normalizeBS #-}

normalizeBS :: B.ByteString -> B.ByteString
normalizeBS bs = B.map toLower8' bs
    where
      toLower8' :: Word8 -> Word8
      toLower8' w
          |  65  <= w && w <=  90 ||
             192 <= w && w <= 214 ||
             216 <= w && w <= 222 = w + 32
          | otherwise             = w
