{-# LANGUAGE OverloadedStrings #-}

module Inngest.Signing
  ( stripKeyPrefix
  , hashSigningKey
  , hashEventKey
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray.Encoding (convertToBase, convertFromBase, Base(Base16))

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
    Left _        -> hexOf (stripKeyPrefix k)   -- non-hex key: hash bytes directly
    Right decoded -> hexOf decoded

-- | SHA256 of the raw utf8 key bytes, hex-encoded.
hashEventKey :: ByteString -> ByteString
hashEventKey = hexOf
