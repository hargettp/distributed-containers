-----------------------------------------------------------------------------
-- |
-- Module      :  TestVariable
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

module TestVariable (
    tests
) where

-- local imports

import Distributed.Data.Container
import qualified Distributed.Data.Variable as V

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
    testCase "var-1server" test1Server,
    testCase "var-3server" test3Servers
    ]

test1Server :: Assertion
test1Server = timeBound (1000000) $ do
    let name = servers !! 0
        cfg = newTestConfiguration [name]
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name $ \endpoint -> do
            withVariableServer endpoint cfg name (0 :: Int) $ \vVariable -> causally $ do
                value1 <- V.get vVariable
                liftIO $ assertEqual "Initial value shoould be 0" 0 value1
                V.set (value1 + 2) vVariable
                value2 <- V.get vVariable
                liftIO $ assertEqual "Second value shoould be 2" 2 value2

test3Servers :: Assertion
test3Servers = with3VariableServers (0 :: Int) $ \vVariables -> timeBound (30 * 1000000) $
    causally $ do
        let [vVar1,vVar2,_] = vVariables
        value1 <- V.get vVar1
        liftIO $ assertEqual "Initial value shoould be 0" 0 value1
        V.set (value1 + 2) vVar1
        value2 <- V.get vVar1
        liftIO $ assertEqual "Second value shoould be 2" 2 value2
        value3 <- V.get vVar2
        liftIO $ assertEqual "Values from both variables should be same" value2 value3

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

withVariableServer :: (Serialize v) => Endpoint -> RaftConfiguration -> Name -> v -> (V.Variable v -> IO ()) -> IO ()
withVariableServer endpoint cfg name initialState fn = do
    initialLog <- V.mkVariableLog
    V.withVariable endpoint cfg name initialLog initialState fn

with3VariableServers :: (Serialize v) => v -> ([V.Variable v ] -> IO ()) -> IO ()
with3VariableServers initialState fn = do
    let names = take 3 servers
        [name1,name2,name3] = names
        cfg = newTestConfiguration names
    withTransport newMemoryTransport $ \transport ->
        withEndpoint transport name1 $ \endpoint1 -> do
            withVariableServer endpoint1 cfg name1 initialState $ \vVariable1 -> do
                withEndpoint transport name2 $ \endpoint2 -> do
                    withVariableServer endpoint2 cfg name2 initialState $ \vVariable2 -> do
                        withEndpoint transport name3 $ \endpoint3 -> do
                            withVariableServer endpoint3 cfg name3 initialState $ \vVariable3 -> do
                                fn [vVariable1,vVariable2,vVariable3]
