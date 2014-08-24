{-# LANGUAGE ExistentialQuantification #-}

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
    containerDataAt,
    withContainer,
    Causal(..),
    liftIO,
    waitUntil,
    perform,
    causally
) where

-- local imports

-- external imports

import Control.Applicative
import Control.Concurrent.STM
import Control.Consensus.Raft

import Network.Endpoints

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Container l e v = Container {
    containerClient :: TVar Client,
    containerRaft :: Raft l e v
}

containerData :: Container l e v -> IO v
containerData container = atomically $ do
        raft <- readTVar $ raftContext $ containerRaft container
        return $ raftStateData $ raftState raft

containerDataAt :: (RaftLog l e v) => Container l e v -> Index -> IO (v,Index)
containerDataAt container time = atomically $ do
        raft <- readTVar $ raftContext $ containerRaft container
        let now = (lastCommitted $ raftLog raft)
        if now < time
            then retry
            else return (raftStateData $ raftState raft,now)

withContainer :: (RaftLog l e v) => Endpoint -> RaftConfiguration -> Name -> l -> v -> (Container l e v -> IO ()) -> IO ()
withContainer endpoint cfg name initialLog initialData fn = do
    let initialState = mkRaftState initialData cfg name
    withConsensus endpoint name initialLog initialState $ \vRaft -> do
        let client = mkClient endpoint name cfg
        vClient <- atomically $ newTVar client
        fn $ Container vClient vRaft

waitUntil :: (RaftLog l e v) => Container l e v -> Index -> IO Index
waitUntil container index = atomically $ do
    raft <- readTVar $ raftContext $ containerRaft container
    let now = lastCommitted $ raftLog raft
    if now < index
        then retry
        else return now

perform :: (RaftLog l e v) => RaftAction e -> Container l e v -> IO Index
perform action container = do
    client <- atomically $ readTVar $ containerClient container
    (time,newClient) <- performAction client action
    atomically $ writeTVar (containerClient container) newClient
    -- here we wait until the action is committed before continuing
    waitUntil container $ logIndex time

{-|
Ordering distributed operations together with local operations can be tricky: a distributed
operation may not have completed before an application is ready to perform a local operation
that should causally occur after the distributed operation has successfully completed.
Thus, the `Causal` monad: it ensures causal ordering of operations, so that one
can write sequential code with guarantees of "happens before" ordering. That is, lines
earlier in a monad are guaranteed to execute before lines later in the monad. Pragmatically,
this means that read operations ordered after a write operation are guaranteed to see the 
effects of the write operation.

Note that the `Causal` monad provides no guarantee of atomicity, however: while reads ordered
after writes are guaranteed to see the effects of the earlier write, they are not guaranteed
to only see the effects of the earlier write, as other threads (or other members of the cluster)
may have succeeded in sequence write before the read is performed.
-}
newtype Causal a = Causal (Index -> IO (a, Index))

liftIO :: IO a -> Causal a
liftIO action = Causal $ \index -> do
    val <- action
    return $ (val,index)

causally :: Causal a -> IO a
causally (Causal action) = do
    (value,_) <- action $ (-1)
    return value

instance Functor Causal where
    -- fmap :: (a -> b) -> f a -> f b
    fmap ab (Causal fa) = Causal $ \time -> do
        (a,aTime) <- fa time
        return $ (ab a,aTime)

instance Applicative Causal where
    pure = return
    -- (<*>) :: f (a -> b) -> f a -> f b
    Causal fab <*> Causal fa = Causal $ \time -> do
        (fn,fnTime) <- fab time
        (a,aTime) <- fa fnTime
        return $ (fn a,aTime)

instance Monad Causal where
    (Causal ma) >>= amb = Causal $ \time -> do
        (val,newTime) <- ma time
        let Causal mb = amb val
        mb newTime
    return a = Causal $ \time -> return (a,time)


