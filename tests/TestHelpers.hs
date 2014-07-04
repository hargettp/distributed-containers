-----------------------------------------------------------------------------
-- |
-- Module      :  TestHelpers
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

module TestHelpers (
    timeBound,
    servers,
    withTransport,
    withEndpoint,
    newTestConfiguration,
    pause,
    waitForLeader
) where

-- local imports

import Distributed.Data.Container

-- external imports

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Consensus.Raft
import Control.Exception

import Debug.Trace

import Network.Endpoints

import Test.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

timeBound :: Int -> IO () -> IO ()
timeBound time fn = do
    outcome <- race (threadDelay time)
        fn
    assertEqual "Test should not block" (Right ()) outcome

--------------------------------------------------------------------------------
-- Consistency
--------------------------------------------------------------------------------

waitForLeader :: Integer -> Integer -> [Container l e v] -> IO (Maybe Name)
waitForLeader maxCount attempt vContainers = do
    let vRafts = map containerRaft vContainers
    leaders <- allLeaders vRafts
    let leader = leaders !! 0
    if maxCount <= 0
        then do
            assertBool ("No leader found after " ++ (show (attempt - 1)) ++ " rounds: " ++ (show leaders)) False
            return Nothing
        else do
            pause
            if (leader /= Nothing) && (all (== leader) leaders)
                then do
                    let msg = "After " ++ (show attempt) ++ " rounds, the leader is " ++ (show leader)
                    if attempt > 3
                        then traceIO msg
                        else traceIO msg
                    return leader
                else
                    waitForLeader (maxCount - 1) (attempt + 1) vContainers

allLeaders :: [Raft l e v] -> IO [Maybe Name]
allLeaders vRafts = do
    rafts <- mapM (\vRaft -> atomically $ readTVar $ raftContext vRaft) vRafts
    let leaders = map (clusterLeader . clusterConfiguration . raftStateConfiguration . raftState) rafts
    return leaders

--------------------------------------------------------------------------------
-- Initial configuration
--------------------------------------------------------------------------------

servers :: [Name]
servers = ["server1","server2","server3"]

withEndpoint :: Transport -> Name -> (Endpoint -> IO ()) -> IO ()
withEndpoint transport name fn = do
    endpoint <- newEndpoint [transport]
    bindEndpoint_ endpoint name
    finally (fn endpoint)
        (unbindEndpoint_ endpoint name)

withTransport :: IO Transport -> (Transport -> IO ()) -> IO ()
withTransport factory fn = do
    bracket factory
        shutdown
        fn

newTestConfiguration :: [Name] -> RaftConfiguration
newTestConfiguration members = (mkRaftConfiguration members) {clusterTimeouts = testTimeouts}

testTimeouts :: Timeouts
testTimeouts = timeouts (25 * 1000)

pause :: IO ()
pause = threadDelay serverTimeout

serverTimeout :: Timeout
serverTimeout = 2 * (snd $ timeoutElectionRange testTimeouts)

{-
newSocketTestConfiguration :: [Name] -> RaftConfiguration
newSocketTestConfiguration members = (mkRaftConfiguration members) {clusterTimeouts = testSocketTimeouts}

testSocketTimeouts :: Timeouts
testSocketTimeouts = timeouts (150 * 1000)

-}