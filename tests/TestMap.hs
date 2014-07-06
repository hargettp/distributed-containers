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

import Control.Consensus.Raft

import Distributed.Data.Container
import qualified Data.Map as M
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
    testCase "map-empty" testEmpty,
    testCase "map-1server" test1Server,
    testCase "map-3servers" test3Servers
    ]

testEmpty :: Assertion
testEmpty = do
    dmap <- DM.empty
    let size = M.size dmap
    assertBool "Should have a map" True
    assertEqual "Should have a an empty map" 0 size

test1Server :: Assertion
test1Server = timeBound (1000000) $ do
    let name = servers !! 0
        cfg = newTestConfiguration [name]
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name $ \endpoint -> do
            withMapServer endpoint cfg name $ \vMap -> causally $ do
                liftIO $ assertBool "we made it!" True
                _ <- DM.insert "foo" "bar" vMap
                value <- DM.lookup "foo" vMap
                liftIO $ assertEqual "Lookup value should equal insert value" (Just "bar") value
                size <- DM.size vMap
                liftIO $ assertEqual "Size should be 1" 1 size
                _ <- DM.delete "foo" vMap
                deletedValue <- DM.lookup "foo" vMap
                liftIO $ assertEqual "Lookup value should be nothing" Nothing deletedValue
                deletedSize <- DM.size vMap
                liftIO $ assertEqual "Size should be 0" 0 deletedSize

test3Servers :: Assertion
test3Servers = timeBound (30 * 1000000) $ do
    with3MapServers $ \vMaps -> causally $ do
        let [vMap1,vMap2,_] = vMaps
        liftIO $ assertBool "we made it!" True
        DM.insert "foo" "bar" vMap1
        value1 <- DM.lookup "foo" vMap1
        liftIO $ assertEqual "Lookup value1 should equal insert value" (Just "bar") value1
        value2 <- DM.lookup "foo" vMap2
        liftIO $ assertEqual "Lookup value2 should equal insert value" (Just "bar") value2
        size <- DM.size vMap2
        liftIO $ assertEqual "Size should be 1" 1 size
        DM.delete "foo" vMap1
        deletedValue <- DM.lookup "foo" vMap2
        liftIO $ assertEqual "Lookup value should be nothing" Nothing deletedValue
        deletedSize <- DM.size vMap2
        liftIO $ assertEqual "Size should be 0" 0 deletedSize

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withMapServer :: (Ord k,Serialize k,Serialize v) => Endpoint -> RaftConfiguration -> Name -> (DM.Map k v -> IO ()) -> IO ()
withMapServer endpoint cfg name fn = do
    initialLog <- DM.mkMapLog
    initialState <- DM.empty
    DM.withMap endpoint cfg name initialLog initialState fn

with3MapServers :: (Ord k,Serialize k,Serialize v) => ([DM.Map k v ] -> IO ()) -> IO ()
with3MapServers fn = do
    let names = take 3 servers
        [name1,name2,name3] = names
        cfg = newTestConfiguration names
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name1 $ \endpoint1 -> do
            withMapServer endpoint1 cfg name1 $ \vMap1 -> do
                withEndpoint transport name2 $ \endpoint2 -> do
                    withMapServer endpoint2 cfg name2 $ \vMap2 -> do
                        withEndpoint transport name3 $ \endpoint3 -> do
                            withMapServer endpoint3 cfg name3 $ \vMap3 -> do
                                fn [vMap1,vMap2,vMap3]
