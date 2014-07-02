-----------------------------------------------------------------------------
-- |
-- Module      :  TestMap
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

module TestMap (
    tests
) where

-- local imports

import qualified Distributed.Data.Map as DM

import TestHelpers

-- external imports

import qualified Data.Map as M

import Network.Transport.Memory

import Prelude hiding (log)

import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

tests :: IO [Test.Framework.Test]
tests = return [
    testCase "empty" testEmpty,
    testCase "1server" test1Server
    ]

testEmpty :: Assertion
testEmpty = do
    dmap <- DM.empty
    let size = M.size dmap
    assertBool "Should have a map" True
    assertEqual "Should have a an empty map" 0 size

test1Server :: Assertion
test1Server = do
    let name = servers !! 0
        cfg = newTestConfiguration [name]
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name $ \endpoint -> do
            initialLog <- DM.mkMapLog
            initialState <- DM.empty :: IO (DM.MapState String String)
            DM.withMap endpoint cfg name initialLog initialState $ \vMap -> do
                assertBool "we made it!" True
                DM.insert "foo" "bar" vMap
                value <- DM.lookup "foo" vMap
                assertEqual "Lookup value should equal insert value" (Just "bar") value
                size <- DM.size vMap
                assertEqual "Size should be 1" 1 size
                DM.delete "foo" vMap
                deletedValue <- DM.lookup "foo" vMap
                assertEqual "Lookup value should be nothing" Nothing deletedValue
                deletedSize <- DM.size vMap
                assertEqual "Size should be 0" 0 deletedSize

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-
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

newSocketTestConfiguration :: [Name] -> RaftConfiguration
newSocketTestConfiguration members = (mkRaftConfiguration members) {clusterTimeouts = testSocketTimeouts}

pause :: IO ()
pause = threadDelay serverTimeout

testSocketTimeouts :: Timeouts
testSocketTimeouts = timeouts (150 * 1000)

serverTimeout :: Timeout
serverTimeout = 2 * (snd $ timeoutElectionRange testTimeouts)
-}