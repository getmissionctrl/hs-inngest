{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Inngest.Signing
  ( stripKeyPrefix
  , hashSigningKey
  , hashEventKey
  , canonicalize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Char (ord)
import Text.Printf (printf)

-- | Drop a leading @signkey-<word>-@ prefix if present.
stripKeyPrefix :: ByteString -> ByteString
stripKeyPrefix k =
  case BC.stripPrefix "signkey-" k of
    Nothing   -> k
    Just rest -> case BC.elemIndex '-' rest of
      Nothing -> k
      Just i  -> BC.drop (i + 1) rest

hexOf :: ByteString -> ByteString
hexOf = convertToBase Base16 . hashWith SHA256

-- | SHA256 of the hex-decoded remainder after stripping the prefix, hex-encoded.
hashSigningKey :: ByteString -> ByteString
hashSigningKey k =
  case convertFromBase Base16 (stripKeyPrefix k) of
    Left _                    -> hexOf (stripKeyPrefix k)   -- non-hex key: hash bytes directly
    Right (decoded :: ByteString) -> hexOf decoded

-- | SHA256 of the raw utf8 key bytes, hex-encoded.
hashEventKey :: ByteString -> ByteString
hashEventKey = hexOf

-- | RFC 8785 JSON Canonicalization Scheme.
canonicalize :: Value -> ByteString
canonicalize = BL.toStrict . B.toLazyByteString . go
  where
    go :: Value -> B.Builder
    go Null       = B.string7 "null"
    go (Bool b)   = B.string7 (if b then "true" else "false")
    go (String s) = jcsString s
    go (Number n) = B.string7 (jcsNumber n)
    go (Array xs) =
      B.char7 '[' <> commaSep (map go (foldrToList xs)) <> B.char7 ']'
    go (Object o) =
      let kvs = sortBy (comparing (utf16Units . fst))
                       [ (K.toText k, v) | (k, v) <- KM.toList o ]
      in B.char7 '{'
         <> commaSep [ jcsString k <> B.char7 ':' <> go v | (k, v) <- kvs ]
         <> B.char7 '}'

    foldrToList = foldr (:) []
    commaSep = mconcat . intersperseB (B.char7 ',')
    intersperseB _   []     = []
    intersperseB sep (y:ys) = y : concatMap (\z -> [sep, z]) ys

-- | Key ordering is by UTF-16 code units (RFC 8785 §3.2.3).
utf16Units :: Text -> [Int]
utf16Units = concatMap toUnits . T.unpack
  where
    toUnits c
      | ord c <= 0xFFFF = [ord c]
      | otherwise =
          let n = ord c - 0x10000
          in [0xD800 + (n `div` 0x400), 0xDC00 + (n `mod` 0x400)]

-- | Minimal JSON string escaping per RFC 8785 §3.2.2.2.
jcsString :: Text -> B.Builder
jcsString s = B.char7 '"' <> T.foldr (\c acc -> esc c <> acc) mempty s <> B.char7 '"'
  where
    esc c = case c of
      '"'  -> B.string7 "\\\""
      '\\' -> B.string7 "\\\\"
      '\b' -> B.string7 "\\b"
      '\f' -> B.string7 "\\f"
      '\n' -> B.string7 "\\n"
      '\r' -> B.string7 "\\r"
      '\t' -> B.string7 "\\t"
      _ | ord c < 0x20 -> B.string7 (printf "\\u%04x" (ord c))
        | otherwise    -> B.stringUtf8 [c]

-- | ECMAScript number formatting. Integers print without a decimal point;
-- non-integers use the shortest round-tripping decimal. Covers the cases that
-- appear in Inngest payloads; extend against vectors if an exotic float fails.
jcsNumber :: Scientific -> String
jcsNumber n = case floatingOrInteger n of
  Left  (d :: Double)  -> shortestDouble d
  Right (i :: Integer) -> show i
  where
    shortestDouble d =
      let s = show d
      in maybe s id (stripTrailing s)
    stripTrailing s
      | ".0" `isSuffixOfStr` s = Just (take (length s - 2) s)
      | otherwise              = Nothing
    isSuffixOfStr suf str = drop (length str - length suf) str == suf
