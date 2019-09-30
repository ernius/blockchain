{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric, OverloadedStrings  #-}

module Web where

import Prelude                     ((!!), fail)
import Protolude                   hiding (hash, State, ask)
import Servant                     
import GHC.Generics                (Generic)
import qualified Data.HashMap.Strict as H
import Control.Concurrent.STM      (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class      (liftIO)
import Control.Monad.STM           (atomically)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT, mapReaderT)
import Network.Wai                 (Application)
import Network.Wai.Handler.Warp    (run)
import Crypto.PubKey.ECC.ECDSA     (PublicKey(..), PrivateKey(..))
import Data.Aeson                  as A

import Transaction
import Util

type GetTransaction     = Capture "tidx" Text :> Get '[JSON] Transaction
type Broadcast          = ReqBody '[JSON] Transaction :> PostCreated '[JSON] Transaction
type GetAllTransactions = Get '[JSON] [(ByteString,Transaction)]

type TransactionAPI = "transactions" :> (GetTransaction :<|> Broadcast :<|> GetAllTransactions)

transactionAPI :: Proxy TransactionAPI
transactionAPI  = Proxy

-- | fixed random seed, to make deterministc tests
random_seed :: Int
random_seed = 0

get_priv_keys :: Keys
get_priv_keys = createKeysDet random_seed 3

-- | Initial Mocked transactions
-- | t0 = pub1 30 --> pub2 20, pub3 10
-- | t1 = pub2 20 --> pub2 10, pub3 10
-- | balance = pub1 0, pub2 10, pub3 20
get_transactions :: IO Transactions
get_transactions = do
  let [(pub1,priv1),(pub2,priv2),(pub3,_)] = get_priv_keys
  t0 <- transaction priv1 (Transfer pub1 [TIn initial_tidx 30] [TOut pub2 20, TOut pub3 10])
  let t0idx = hashTransaction t0
  t1 <- transaction priv2 (Transfer pub2 [TIn t0idx 0] [TOut pub2 10, TOut pub3 10])
  let t1idx = hashTransaction t1
  return $ H.fromList [(t0idx, t0),(t1idx, t1)]

data State = State
  { transactions :: TVar Transactions
  }

type AppM = ReaderT State Handler

-- | debuging endpoint
getAllTransaction :: AppM [(ByteString, Transaction)]
getAllTransaction = do
  State{transactions = ts} <- ask
  liftIO $ fmap H.toList $ readTVarIO ts

-- | get transaction endpoint
getTransaction :: Text -> AppM Transaction
getTransaction tidx = do
  State{transactions = tsV} <- ask
  ts <- liftIO $ readTVarIO tsV
  d  <- unbase16T tidx
  case H.lookup d ts of
    Just t  -> return t
    Nothing -> lift $ Handler $ throwError $ err400 { errBody = "Transaction not found"}

validations :: [Transactions -> Transaction -> Bool]
validations = [ checkTransactionRefExists
              , checkTransactionRefRecipients
              , checkTransactionBalance
              ]

-- | runs all validations inside atomically which grants the transactions are not updated during checking
validate :: Transaction -> Transactions -> STM Transactions
validate t ts = do
  if and [f ts t | f <- validations] then
    return ts
  else
    fail "Invalid transaction"

-- | broadcast endpoint
broadcast :: Transaction -> AppM Transaction
broadcast t =
  -- verify signature
  if verifyTransactionSignature t && checkTransaction t then do
    State{transactions = tsV} <- ask
    liftIO $ atomically $ readTVar tsV >>= validate t >>= writeTVar tsV . (H.insert (hashTransaction t) t)
    return t
  else
    lift $ Handler $ throwError $ err400 { errBody = "Invalid transaction"}

server :: ServerT TransactionAPI AppM
server = getTransaction :<|> broadcast :<|> getAllTransaction

nt :: State -> AppM a -> Handler a
nt = flip runReaderT

transaction_app :: State -> Application
transaction_app s = serve transactionAPI $ hoistServer transactionAPI (nt s) server

webAppEntry :: IO ()
webAppEntry =
  -- start with 2 mocked transactions
  get_transactions  >>= newTVarIO >>=
  -- run HTTP server
  run 8080 . transaction_app . State

-- | Debugging auxiliar functions
-- | Mocked example transaction
exampleTransaction :: IO ()
exampleTransaction = do
  let ks = get_priv_keys
  ts <- get_transactions
  let pubKey1   = fst $ ks !! 0
  let (Just t0) = find (\ t -> sender (header t) == pubKey1) $ H.elems ts
  let t0Idx = hashTransaction t0
  let (pubKey3, privKey3) = ks !! 2
  let pubKey2 = fst $ ks !! 1
  t      <- transaction privKey3 (Transfer pubKey3 [TIn t0Idx  1] [TOut pubKey3 5, TOut pubKey2 5])
  let tIdx = decodeUtf8 $ base16 $ hashTransaction t
  putStr $ A.encode $ A.toJSON t




