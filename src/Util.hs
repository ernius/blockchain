{-# LANGUAGE DeriveGeneric, OverloadedStrings, StandaloneDeriving #-}

module Util where

import Protolude
import GHC.Generics (Generic)
import Data.Serialize
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BA
import Crypto.Random
import Crypto.Number.Basic
import Crypto.Number.Serialize
import Crypto.PubKey.ECC.Generate (generate)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(SEC_p256k1), Point(..))
import Crypto.PubKey.ECC.ECDSA (sign, verify, PublicKey(..), PrivateKey(..), public_q, Signature(..))
import Crypto.Hash (hashWith, Digest, SHA3_256(..))

type Hash = Digest SHA3_256
type Keys = [(PublicKey,PrivateKey)]

hash :: ByteString -> Hash
hash = hashWith SHA3_256

base16 :: (BA.ByteArrayAccess bin, BA.ByteArray bout) => bin -> bout
base16 = BA.convertToBase BA.Base16
  
unbase16 :: (BA.ByteArrayAccess bin, BA.ByteArray bout) => bin -> Either String bout
unbase16 = BA.convertFromBase BA.Base16

unbase16B :: ByteString -> Either String ByteString
unbase16B = BA.convertFromBase BA.Base16

unbase16T :: (Monad m) => Text -> m ByteString 
unbase16T = either (panic . toS) pure . unbase16B . toS

-- encode64 :: ByteString -> Text
-- encode64 = decodeUtf8 . BS64.encode

-- decode64 :: (Monad m) => Text -> m ByteString
-- decode64 = either (panic . toS) pure . BS64.decode . toS

sign :: PrivateKey -> ByteString -> IO Signature
sign privKey = Crypto.PubKey.ECC.ECDSA.sign privKey SHA3_256

verify :: PublicKey -> Signature -> ByteString -> Bool
verify = Crypto.PubKey.ECC.ECDSA.verify SHA3_256

-- | Create Random keys
createKeys :: IO (PublicKey, PrivateKey)
createKeys = generate (getCurveByName SEC_p256k1)

-- | Deterministicly create n keys given the seed s
createKeysDet :: Int -> Int -> [(PublicKey, PrivateKey)]
createKeysDet s n = fst $ withDRG (drgNewSeed (seedFromInteger 0)) (replicateM n (generate (getCurveByName SEC_p256k1)))

-- | Serialize an Integer
putInteger :: Putter Integer
putInteger n = do
  let nBytes = numBytes n
  putInt64le $ fromIntegral nBytes
  Data.Serialize.putByteString $ i2ospOf_ nBytes n

-- | Deserialize an Integer
-- UNSAFE: vulnerable to long Integer injection attacks
getInteger :: Get Integer
getInteger = do
  nBytes <- fromIntegral <$> getInt64le
  os2ip <$> getByteString nBytes

extractPoint :: PublicKey -> (Integer, Integer)
extractPoint pubkey = (x,y)
  where
    Point x y = public_q pubkey

putPublicKey :: Putter PublicKey
putPublicKey pubKey = do
  let (x,y) = extractPoint pubKey
  putInteger x
  putInteger y

mkPublicKey :: (Integer, Integer) -> PublicKey
mkPublicKey (x,y) = PublicKey (getCurveByName SEC_p256k1) $ Point x y

-- | UNSAFE: Does not check the validity of the point
getPublicKey :: Get PublicKey
getPublicKey = do
  x <- getInteger
  y <- getInteger
  pure $ mkPublicKey (x,y)

instance Serialize PublicKey where
  put = putPublicKey
  get = getPublicKey

deriving instance Generic Signature
instance Serialize Signature

