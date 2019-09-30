{-# LANGUAGE TypeFamilies, DataKinds, DeriveGeneric, TypeOperators, OverloadedStrings  #-}

import           Protolude                 hiding (show)

import qualified Data.HashMap.Strict       as H
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy, responseBody)
import           Network.Wai.Handler.Warp  (run)

import           Servant
import           Servant.Client
import           Servant.Server

import           Test.Hspec

import           Control.Monad.STM            (atomically)
import           Control.Concurrent           (forkIO, killThread)
import           Control.Concurrent.STM.TVar  (newTVarIO)
import           Control.Exception            (bracket)
import           Control.Monad.IO.Class       (liftIO)

import           Web
import           Transaction
import           Util

test_host :: String
test_host = "localhost"

test_port :: Int
test_port = 8888

test_server_url :: String
test_server_url = "http://" ++ test_host ++ ":" ++ show test_port

withTransactionApp :: ((Transactions, Keys) -> IO ()) -> IO ()
withTransactionApp action = do
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  ts  <- get_transactions
  tsV <- newTVarIO ts
  let keys = get_priv_keys
  bracket
    (liftIO $ fmap (\ thId -> (thId, (ts, keys))) $ forkIO $ run test_port $ transaction_app $ State tsV)
    (killThread . fst)
    (action . snd)

businessLogicSpec :: Spec
businessLogicSpec = do
  -- `around` will start our Server before the tests and turn it off after
  around withTransactionApp  $ do
    -- create a test client function
    let getTransactionClient :<|> broadcastClient :<|> getAllTransactionsClient = client transactionAPI
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl $ test_server_url
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl
    -- testing scenarios start here
    describe "get transaction = GET /transactions/:tidx" $ do
      it "should get :tidx transaction" $ \ (ts,_) -> do
        let transaction0    =  (!! 0) $ H.elems ts
        let transaction0Idx = decodeUtf8 $ base16 $ hashTransaction transaction0
        (Right result) <- runClientM (getTransactionClient transaction0Idx) clientEnv
        result `shouldBe` transaction0

      it "should fail when :tidx transaction does not exist" $ \ (ts,_) -> do
        let tidx = "111111"
        (Left (FailureResponse _ response)) <- runClientM (getTransactionClient tidx) clientEnv
        (responseBody response) `shouldBe` "Transaction not found"

    describe "broadcast = POST /transations" $  do
      it "should broadcast a transaction and then be accesible" $ \ (ts, ks) -> do
        -- t3 = (t0) pub3 10 -> pub3 5,pub2 5
        let pubKey1   = fst $ ks !! 0
        let (Just t0) = find (\ t -> sender (header t) == pubKey1) $ H.elems ts
        let t0Idx = hashTransaction t0
        let (pubKey3, privKey3) = ks !! 2
        let pubKey2 = fst $ ks !! 1
        t      <- transaction privKey3 (Transfer pubKey3 [TIn t0Idx  1] [TOut pubKey3 5, TOut pubKey2 5])
        let tIdx = decodeUtf8 $ base16 $ hashTransaction t
        result <- runClientM (broadcastClient t) clientEnv
        result `shouldBe` (Right $ tIdx)
        t' <- runClientM (getTransactionClient tIdx) clientEnv
        t'     `shouldBe` (Right $ t)

      it "should fail as signature is invalid" $ \ (ts, ks) -> do
        -- t3 = (t0) pub3 10 -> pub3 5,pub2 5
        let pubKey1   = fst $ ks !! 0
        let (Just t0) = find (\ t -> sender (header t) == pubKey1) $ H.elems ts
        let t0Idx = hashTransaction t0
        let (pubKey3, privKey3) = ks !! 2
        let pubKey2 = fst $ ks !! 1
        t      <- transaction privKey3 (Transfer pubKey3 [TIn t0Idx  1] [TOut pubKey3 5, TOut pubKey2 5])
        (Left (FailureResponse _ response)) <- runClientM (broadcastClient (t { signature = "bad"})) clientEnv
        (responseBody response) `shouldBe` "Invalid transaction"

spec :: Spec
spec = do
  businessLogicSpec

main :: IO ()
main = hspec spec  
