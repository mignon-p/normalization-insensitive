{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main ( main) where

import           Criterion.Main             ( defaultMain, bench, nf )
import qualified Data.ByteString      as B  ( readFile )
import qualified Data.Unicode.NormalizationInsensitive as NI ( mk )
import qualified NoClass              as NC ( mk )

#if !MIN_VERSION_bytestring(0,10,0)
import Control.DeepSeq ( NFData )
instance NFData ByteString
#endif

main :: IO ()
main = do
  bs <- B.readFile "pg2189.txt"
  defaultMain
    [ bench "no-class"         $ nf (\s -> NC.mk s) bs
    , bench "normalization-insensitive" $ nf (\s -> NI.mk s) bs
    ]
