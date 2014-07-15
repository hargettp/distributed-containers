-----------------------------------------------------------------------------
-- |
-- Module      :  TestQueue
-- Copyright   :  (c) Phil Hargett 2014
-- License     :  MIT (see LICENSE file)
-- 
-- Maintainer  :  phil@haphazardhouse.net
-- Stability   :  experimental
-- Portability :  non-portable (requires STM)
--
-- (..... module description .....)
--
-----------------------------------------------------------------------------

module TestQueue (
    tests
) where

-- local imports

import qualified Distributed.Data.Queue as Q

import Distributed.Data.Container

import TestHelpers

-- external imports

import Control.Consensus.Raft

import Data.Serialize

import Network.Endpoints
import Network.Transport.Memory

import Prelude hiding (log)

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

tests :: IO [Test.Framework.Test]
tests = return [
    testCase "queue-1server" $ test1Server,
    testCase "queue-3server" $ test3Servers
    -- testCase "queue-3server" $ troubleshoot $ test3Servers
    ]

test1Server :: Assertion
test1Server = timeBound (2 * 1000 * 1000) $ do
    let name = servers !! 0
        cfg = newTestConfiguration [name]
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name $ \endpoint -> do
            withQueueServer endpoint cfg name $ \vQueue -> causally $ do
                checkSize "Empty queue should have size 0" 0 vQueue
                Q.enqueue (0 :: Int) vQueue
                checkSize "Queue size should be 1" 1 vQueue
                Q.enqueue 1 vQueue
                checkSize "Queue size should be 2" 2 vQueue
                Q.enqueue 2 vQueue
                checkSize "Queue size should be 3" 3 vQueue
                value1 <- Q.dequeue vQueue
                checkSize "Queue size should again be 2" 2 vQueue
                liftIO $ assertEqual "Initial value should be 0" 0 value1
                value2 <- Q.dequeue vQueue
                checkSize "Queue size should again be 1" 1 vQueue
                liftIO $ assertEqual "Second value should be 1" 1 value2
                value3 <- Q.dequeue vQueue
                checkSize "Queue size should again be 0" 0 vQueue
                liftIO $ assertEqual "Third value should be 2" 2 value3

test3Servers :: Assertion
test3Servers =  with3QueueServers $ \vQueues -> timeBound (30 * 1000000) $
    causally $ do
        let [vQueue1,vQueue2,vQueue3] = vQueues
        checkSize "Empty queue should have size 0" 0 vQueue1
        Q.enqueue (0 :: Int) vQueue1
        checkSize "Queue size should be 1" 1 vQueue1
        Q.enqueue 1 vQueue1
        checkSize "Queue size should be 2" 2 vQueue3
        Q.enqueue 2 vQueue2
        checkSize "Queue size should be 3" 3 vQueue2
        value1 <- Q.dequeue vQueue1
        checkSize "Queue size should again be 2" 2 vQueue2
        liftIO $ assertEqual "Initial value should be 0" 0 value1
        value2 <- Q.dequeue vQueue3
        checkSize "Queue size should again be 1" 1 vQueue2
        liftIO $ assertEqual "Second value should be 1" 1 value2
        value3 <- Q.dequeue vQueue1
        checkSize "Queue size should again be 0" 0 vQueue2
        liftIO $ assertEqual "Third value should be 2" 2 value3

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withQueueServer :: (Serialize v) => Endpoint -> RaftConfiguration -> Name -> (Q.Queue v -> IO ()) -> IO ()
withQueueServer endpoint cfg name fn = do
    initialLog <- Q.mkQueueLog
    initialState <- Q.empty name
    Q.withQueue endpoint cfg name initialLog initialState fn

with3QueueServers :: (Serialize v) => ([Q.Queue v ] -> IO ()) -> IO ()
with3QueueServers fn = do
    let names = take 3 servers
        [name1,name2,name3] = names
        cfg = newTestConfiguration names
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name1 $ \endpoint1 -> do
            withQueueServer endpoint1 cfg name1 $ \vQueue1 -> do
                withEndpoint transport name2 $ \endpoint2 -> do
                    withQueueServer endpoint2 cfg name2 $ \vQueue2 -> do
                        withEndpoint transport name3 $ \endpoint3 -> do
                            withQueueServer endpoint3 cfg name3 $ \vQueue3 -> do
                                fn [vQueue1,vQueue2,vQueue3]

checkSize :: (Serialize v) => String -> Int -> Q.Queue v -> Causal ()
checkSize msg esz q = do
    sz <- Q.size q
    liftIO $ assertEqual msg esz sz
