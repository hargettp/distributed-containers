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
    servers,
    withTransport,
    withEndpoint,
    newTestConfiguration
) where

-- local imports

-- external imports

import Control.Consensus.Raft
import Control.Exception

import Network.Endpoints

--------------------------------------------------------------------------------
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

{-
newSocketTestConfiguration :: [Name] -> RaftConfiguration
newSocketTestConfiguration members = (mkRaftConfiguration members) {clusterTimeouts = testSocketTimeouts}

pause :: IO ()
pause = threadDelay serverTimeout

testSocketTimeouts :: Timeouts
testSocketTimeouts = timeouts (150 * 1000)

serverTimeout :: Timeout
serverTimeout = 2 * (snd $ timeoutElectionRange testTimeouts)
-}