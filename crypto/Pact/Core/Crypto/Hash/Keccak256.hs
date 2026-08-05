{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implementation of the `keccak256` pact native.
module Pact.Core.Crypto.Hash.Keccak256 (Keccak256Error(..), keccak256) where

import Control.Exception (Exception(..), SomeException(..))
import Control.Monad (forM_)
import Control.Exception.Safe (throwM, try)
import Control.DeepSeq
import Data.ByteString(ByteString)
import Data.ByteString.Short qualified as BSS
import Data.Coerce
--import Data.Hash.Class.Mutable (initialize, finalize, updateByteString)
--import Data.Hash.Internal.OpenSSL (OpenSslException(..))
--import Data.Hash.Keccak (Keccak256(..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector (Vector)
import Pact.Core.Crypto.Base64 (encodeBase64UrlUnpadded, decodeBase64UrlUnpadded)
import GHC.Generics(Generic)
import System.IO.Unsafe (unsafePerformIO)

data Keccak256Error
  = Keccak256OpenSslException String
  | Keccak256Base64Exception String
  | Keccak256OtherException String
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Exception)

instance NFData Keccak256Error

keccak256 :: Vector ByteString -> Either Keccak256Error Text
keccak256 bytesArray = Left $ Keccak256OtherException "Not impleementd with WASM"
{-# noinline keccak256 #-}
