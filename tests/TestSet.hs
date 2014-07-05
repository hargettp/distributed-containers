-----------------------------------------------------------------------------
-- |
-- Module      :  TestSet
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

module TestSet (
    tests
) where

-- local imports

import Distributed.Data.Container
import qualified Distributed.Data.Set as S

import TestHelpers

-- external imports

import Control.Consensus.Raft

import Data.Serialize
import qualified Data.Set as Set

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
    testCase "set-1server" test1Server,
    testCase "set-3server" test3Servers
    ]

test1Server :: Assertion
test1Server = timeBound (1000000) $ do
    let name = servers !! 0
        cfg = newTestConfiguration [name]
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name $ \endpoint -> do
            withSetServer endpoint cfg name (Set.empty :: Set.Set String) $ \vSet -> causally $ do
                elements <- S.elems vSet
                liftIO $ assertBool "Initial set should be empty" $ null elements
                S.insert "foo" vSet
                isMember1 <- S.member "foo" vSet
                liftIO $ assertBool "Foo should be an element" isMember1
                S.insert "bar" vSet
                size <- S.size vSet
                liftIO $ assertEqual "Set should have size 2" 2 size

test3Servers :: Assertion
test3Servers = with3SetServers (Set.empty :: Set.Set String) $ \vSets -> timeBound (30 * 1000000) $
    causally $ do
        _ <- liftIO $ waitForLeader vSets
        let [vSet1,vSet2,_] = vSets
        elements <- S.elems vSet1
        liftIO $ assertBool "Initial set should be empty" $ null elements
        S.insert "foo" vSet1
        isMember1 <- S.member "foo" vSet1
        liftIO $ assertBool "Foo should be an element" isMember1
        S.insert "bar" vSet1
        isMember2 <- S.member "foo" vSet2
        liftIO $ assertBool "Foo should be an element of vSet2" isMember2
        isMember3 <- S.member "bar" vSet2
        liftIO $ assertBool "bar should be an element of vSet2" isMember3
        size1 <- S.size vSet1
        size2 <- S.size vSet2
        liftIO $ assertEqual "Set2 should have size 2" 2 size2
        liftIO $ assertEqual "Sets should have same size" size1 size2

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withSetServer :: (Ord v,Serialize v) => Endpoint -> RaftConfiguration -> Name -> Set.Set v -> (S.Set v -> IO ()) -> IO ()
withSetServer endpoint cfg name initialState fn = do
    initialLog <- S.mkSetLog
    S.withSet endpoint cfg name initialLog initialState fn

with3SetServers :: (Ord v,Serialize v) => Set.Set v -> ([S.Set v ] -> IO ()) -> IO ()
with3SetServers initialState fn = do
    let names = take 3 servers
        [name1,name2,name3] = names
        cfg = newTestConfiguration names
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name1 $ \endpoint1 -> do
            withSetServer endpoint1 cfg name1 initialState $ \vSet1 -> do
                withEndpoint transport name2 $ \endpoint2 -> do
                    withSetServer endpoint2 cfg name2 initialState $ \vSet2 -> do
                        withEndpoint transport name3 $ \endpoint3 -> do
                            withSetServer endpoint3 cfg name3 initialState $ \vSet3 -> do
                                fn [vSet1,vSet2,vSet3]
