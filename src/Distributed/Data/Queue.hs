{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distributed.Data.Queue
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

module Distributed.Data.Queue (
    Queue,
    QueueLog,
    mkQueueLog,
    QueueState,
    empty,
    withQueue,
    enqueue,
    dequeue,
    size
) where

-- local imports

import qualified Data.Fifo as F

import Distributed.Data.Container

-- external imports

import Control.Applicative hiding (empty)
import Control.Concurrent.STM
import Control.Consensus.Raft

import Data.Serialize

import Network.Endpoints

import Prelude hiding (log,lookup)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

data QueueCommand v = (Serialize v) => Enqueue v | Dequeue Name

deriving instance (Eq v) => Eq (QueueCommand v)
deriving instance (Show v) => Show (QueueCommand v)

instance (Serialize v) => Serialize (QueueCommand v) where
    get = do
        kind <- getWord8
        case kind of
            0 -> Enqueue <$> get
            _ -> Dequeue <$> get
    put (Enqueue value) = do
        putWord8 0
        put value
    put (Dequeue name) = do
        putWord8 1
        put name

data QueueState v = (Serialize v) => QueueState {
    localName :: Name,
    localQueue :: F.Fifo v,
    sharedQueue :: F.Fifo v
}

deriving instance (Eq v) => Eq (QueueState v)
deriving instance (Show v) => Show (QueueState v)

instance (Serialize v) => Serialize (QueueState v) where
    get = do
        name <- get
        local <- get
        shared <- get
        return $ QueueState name local shared

    put queue = do
        put $ localName queue
        put $ localQueue queue
        put $ sharedQueue queue

instance State (QueueState v) IO (QueueCommand v) where

    canApplyEntry _ _ = return True

    applyEntry initial (Enqueue value) = do
        let newQueue = initial {sharedQueue = F.enqueue (sharedQueue initial) value}
        return $ newQueue

    applyEntry initial (Dequeue name) = do
        let (maybeValue,newShared) = F.dequeue (sharedQueue initial)
            in case maybeValue of
                Just value -> if name == (localName initial)
                    then return $ initial {sharedQueue = newShared,
                                        localQueue = F.enqueue (localQueue initial) value}
                    else return $ initial {sharedQueue = newShared}
                Nothing -> return $ initial {sharedQueue = newShared}

type QueueLog v = ListLog (QueueCommand v) (QueueState v)

mkQueueLog :: (Serialize v) => IO (QueueLog v)
mkQueueLog = mkListLog

empty :: (Serialize v) => Name -> IO (QueueState v)
empty name = return $ QueueState {
    localName = name,
    localQueue = F.empty,
    sharedQueue = F.empty
    }

type Queue v = Container (QueueLog v) (QueueCommand v) (QueueState v)

withQueue :: (Serialize v) => Endpoint -> RaftConfiguration -> Name -> QueueLog v -> QueueState v -> (Queue v -> IO ()) -> IO ()
withQueue = withContainer

enqueue :: (Serialize v) => v -> Queue v -> Causal ()
enqueue value queue = Causal $ \_ -> do
    now <- perform (Cmd $ Enqueue value) queue
    return ((),now)

dequeue :: (Serialize v) => Queue v -> Causal v
dequeue queue = Causal $ \_ -> do
    q <- containerData queue
    let name = localName q
    later <- perform (Cmd $ Dequeue name) queue
    (latest,now) <- containerDataAt queue later
    value <- atomically $ do
        case F.dequeue $ localQueue latest of
            (Nothing,_) -> retry
            (Just value,newQueue) -> do
                modifyTVar (raftContext $ containerRaft queue) $ \oldRaft -> setRaftData (latest {localQueue = newQueue}) oldRaft
                return value
    return (value,now)

size :: (Serialize v) => Queue v -> Causal Int
size queue = Causal $ \index -> do
    (q,now) <- containerDataAt queue index
    return (F.size $ sharedQueue q,now)
