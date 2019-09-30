{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, OverloadedStrings, DeriveAnyClass  #-}

module Transaction where

import Prelude                       ((!!))
import Protolude                     hiding (hash)
import GHC.Generics                  (Generic)
import qualified Data.Serialize      as S
import Data.Aeson                    as A
import qualified Data.HashMap.Strict as H
import Crypto.PubKey.ECC.ECDSA       (PublicKey(..), PrivateKey(..))

import Util

type TransactionIdx = Hash

data TIn = TIn
  {  tidx     :: ByteString -- transaction source hash
  ,  position :: Int        -- position in tout list
  } deriving (Eq, Show, Generic, S.Serialize)

instance FromJSON TIn
instance ToJSON   TIn

data TOut = TOut
  {  recipient :: PublicKey -- public key of the transaction destin
  ,  amountOut :: Int       
  } deriving (Eq, Show, Generic, S.Serialize)

instance FromJSON TOut
instance ToJSON   TOut

data TransactionHeader = Transfer
    { sender  :: PublicKey -- Redundant sender pubkey is TOuts's recipient referenced in tin list
    , tin     :: [TIn]     -- transaction inputs
    , tout    :: [TOut]    -- transaction outputs
    } deriving (Eq, Show, Generic, S.Serialize)

instance A.ToJSON PublicKey where
  toJSON k = object
    [ "x" .= (x :: Integer)
    , "y" .= (y :: Integer)
    ]
    where
      (x, y) = extractPoint k

instance A.FromJSON PublicKey where
  parseJSON (Object v) = curry mkPublicKey <$> v .: "x" <*> v .: "y"

instance A.ToJSON   TransactionHeader
instance A.FromJSON TransactionHeader

hashTxHeader :: TransactionHeader -> ByteString
hashTxHeader = base16 . hash . S.encode

data Transaction = Transaction
  { header    :: TransactionHeader
  , signature :: ByteString
  } deriving (Eq, Show, Generic, S.Serialize)

instance FromJSON ByteString where
  parseJSON (A.String s) = unbase16T s

instance ToJSON ByteString where
  toJSON = String . decodeUtf8 . base16  

-- Data.Aeson.decode (Data.Aeson.encode (Data.ByteString.Char8.pack "0")) :: Maybe ByteString
-- Just "0"

instance FromJSON Transaction
instance ToJSON   Transaction

-- | Transaction Idx (tidx)
-- | Hash of the unsigned content of the transaction, that is, the header
hashTransaction :: Transaction -> ByteString
hashTransaction = hashTxHeader . header

-- | Create transaction, given a private key to sign the transactin header
transaction :: PrivateKey -> TransactionHeader -> IO Transaction
transaction privKey th = do
  thSig <- sign privKey $ S.encode th
  pure $ Transaction th $ S.encode thSig

-- | Verify signature of a transaction
verifyTransactionSignature :: Transaction -> Bool
verifyTransactionSignature (Transaction th@(Transfer f _ _) sig) =
  case (S.decode sig) of
    Left _  -> False
    Right d -> verify f d (S.encode th)

-- | Basic checks
checkTransaction  :: Transaction -> Bool
checkTransaction (Transaction (Transfer _ [] _)  _) = False
checkTransaction (Transaction (Transfer _ _ out) _) = (sum $ map amountOut out) > 0

type Transactions = H.HashMap ByteString Transaction

-- | Transaction input references checks
checkTransactionRefExists :: Transactions -> Transaction -> Bool
checkTransactionRefExists ts (Transaction (Transfer sender ins _) _) = and [ ref `H.member` ts | ref <- map tidx ins ]

-- | Transaction input references have the same recipient equals to the sender
checkTransactionRefRecipients :: Transactions -> Transaction -> Bool
checkTransactionRefRecipients ts (Transaction (Transfer sender ins _) _) = and [ sender == recipient | (TOut recipient _) <- getOuts ts ins ]

-- | Transaction inputs sum equals ouputs sum
checkTransactionBalance :: Transactions -> Transaction -> Bool
checkTransactionBalance ts (Transaction (Transfer _ ins out) _) = (sum $ map amountOut $ getOuts ts ins) == (sum $ map amountOut out)

getOuts :: Transactions -> [TIn] -> [TOut]
getOuts ts ins = [ (!! p)  $ tout $ header $ ts H.! ref   | (TIn ref p) <- ins ]

-- | consider "0" a special tidx referencing no transaction in a Tin, its Tin's position field is the amount of initial coins
initial_tidx :: ByteString
initial_tidx = "0"

-- | Mocked example transaction
exampleTransaction :: IO ()
exampleTransaction = do
  let [(pub1, priv1), (pub2, _)] = createKeysDet 1 2
  t <- transaction priv1 (Transfer pub1 [TIn initial_tidx 20] [TOut pub2 20]) -- 
  putStr $ A.encode $ A.toJSON t

