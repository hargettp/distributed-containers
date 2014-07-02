-----------------------------------------------------------------------------
-- |
-- Module      :  Distributed.Data.Container
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

module Distributed.Data.Container (
    Container(..),
    containerData,
    withContainer
) where

-- local imports

-- external imports

import Control.Concurrent.STM
import Control.Consensus.Raft

import Network.Endpoints

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Container l e v = Container {
    containerClient :: Client,
    containerRaft :: Raft l e v
}

containerData :: Container l e v -> IO v
containerData container = atomically $ do
        raft <- readTVar $ raftContext $ containerRaft container
        return $ raftStateData $ raftState raft

withContainer :: (RaftLog l e v) => Endpoint -> RaftConfiguration -> Name -> l -> v -> (Container l e v -> IO ()) -> IO ()
withContainer endpoint cfg name initialLog initialData fn = do
    let initialState = mkRaftState initialData cfg name
    withConsensus endpoint name initialLog initialState $ \vRaft -> do
        let client = newClient endpoint name cfg
        fn $ Container client vRaft

