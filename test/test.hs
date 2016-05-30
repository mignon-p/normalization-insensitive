module Main ( main ) where

import           Data.ByteString                    ( ByteString )
import qualified Data.ByteString.Lazy       as Lazy ( ByteString )
import           Data.Hashable                      ( hash )
import           Data.Text                          ( Text )
import qualified Data.Text                  as T    ( pack )
import qualified Data.Text.Encoding         as T    ( encodeUtf8 )
import qualified Data.Text.Lazy             as Lazy ( Text )
import qualified Data.Text.Lazy             as TL   ( pack )
import qualified Data.Text.Lazy.Encoding    as TL   ( encodeUtf8 )
import qualified Data.Unicode.NormalizationInsensitive as NI ( mk )
import           Test.Framework                     ( defaultMain, testGroup )
import           Test.Framework.Providers.HUnit     ( testCase )
import           Test.HUnit                         ( assertBool, assertEqual )

main :: IO ()
main = defaultMain
  [ testGroup "sensitive"
    [ testCase "String"          $ assertBool "" (nfcStr  /= nfdStr)
    , testCase "ByteString"      $ assertBool "" (nfcBs   /= nfdBs)
    , testCase "Lazy.ByteString" $ assertBool "" (nfcLBs  /= nfdLBs)
    , testCase "Text"            $ assertBool "" (nfcTxt  /= nfdTxt)
    , testCase "Lazy.Text"       $ assertBool "" (nfcLTxt /= nfdLTxt)
    ]
  , testGroup "insensitive"
    [ testCase "String"          $ assertEqual "" (NI.mk nfcStr)  (NI.mk nfdStr)
    , testCase "ByteString"      $ assertEqual "" (NI.mk nfcBs)   (NI.mk nfdBs)
    , testCase "Lazy.ByteString" $ assertEqual "" (NI.mk nfcLBs)  (NI.mk nfdLBs)
    , testCase "Text"            $ assertEqual "" (NI.mk nfcTxt)  (NI.mk nfdTxt)
    , testCase "Lazy.Text"       $ assertEqual "" (NI.mk nfcLTxt) (NI.mk nfdLTxt)
    ]
   , testGroup "hash"
    [ testCase "String"          $ assertEqual "" (hash $ NI.mk nfcStr)  (hash $ NI.mk nfdStr)
    , testCase "ByteString"      $ assertEqual "" (hash $ NI.mk nfcBs)   (hash $ NI.mk nfdBs)
    , testCase "Lazy.ByteString" $ assertEqual "" (hash $ NI.mk nfcLBs)  (hash $ NI.mk nfdLBs)
    , testCase "Text"            $ assertEqual "" (hash $ NI.mk nfcTxt)  (hash $ NI.mk nfdTxt)
    , testCase "Lazy.Text"       $ assertEqual "" (hash $ NI.mk nfcLTxt) (hash $ NI.mk nfdLTxt)
    ]
  ]


nfcLTxt :: Lazy.Text
nfcLTxt = TL.pack nfcStr

nfcTxt :: Text
nfcTxt = T.pack nfcStr

nfcLBs :: Lazy.ByteString
nfcLBs = TL.encodeUtf8 nfcLTxt

nfcBs :: ByteString
nfcBs = T.encodeUtf8 nfcTxt

nfcStr :: String
nfcStr = "C\233sar E. Ch\225vez"


nfdLTxt :: Lazy.Text
nfdLTxt = TL.pack nfdStr

nfdTxt :: Text
nfdTxt = T.pack nfdStr

nfdLBs :: Lazy.ByteString
nfdLBs = TL.encodeUtf8 nfdLTxt

nfdBs :: ByteString
nfdBs = T.encodeUtf8 nfdTxt

nfdStr :: String
nfdStr = "Ce\769sar E. Cha\769vez"
